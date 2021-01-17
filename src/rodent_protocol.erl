-module(rodent_protocol).
-behaviour(gen_server).

-export([start_link/3]).
-include_lib("kernel/include/file.hrl").


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

start_link(Ref, Transport, Options) ->
    Args = [Ref, Transport, Options],
    {ok, proc_lib:spawn_link(fun() -> init(Args) end)}.

%%% gen_server callbacks

init([Ref, Transport, Options]) ->
    {ok, Socket} = ranch:handshake(Ref),
    Transport:setopts(Socket, [{active, true}]),
    State = Options#{
                     socket => Socket,
                     transport => Transport,
                     buffer => <<>>,
                     routes => maps:get(routes, Options, [])
                    },
    gen_server:enter_loop(?MODULE, [], State).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({tcp_closed, Socket}, State = #{socket := Socket}) ->
    {stop, {shutdown, closed}, State};
handle_info({tcp_error, Socket, Reason}, State = #{socket := Socket}) ->
    {stop, Reason, State};
handle_info({tcp, Socket, Data}, State = #{socket := Socket}) ->
    Buffer = maps:get(buffer, State),
    parse(State#{buffer => <<Buffer/bytes, Data/bytes>>}).

terminate(normal, State) ->
    access_log(State);
terminate(_Reason, _State) ->
    ok.


%%% Internal functions

parse(State = #{query := Query, buffer := Buffer}) ->
    parse_query(Buffer, Query, State);
parse(State = #{selector := Selector, buffer := Buffer}) ->
    parse_selector(Buffer, Selector, State);
parse(State = #{buffer := Buffer}) ->
    parse_selector(Buffer, <<>>, State#{buffer => <<>>}).

parse_selector(<<"\r\n", Rest/bytes>>, Acc, State) ->
    Selector = uri_string:normalize(#{path => Acc}),
    select(State#{selector => Selector, buffer => Rest});
parse_selector(<<"\t", Rest/bytes>>, Acc, State) ->
    Selector = uri_string:normalize(#{path => Acc}),
    parse_query(Rest, <<>>, State#{selector => Selector});
parse_selector(<<C, Rest/bytes>>, Acc, State) ->
    parse_selector(Rest, <<Acc/bytes, C>>, State);
parse_selector(<<>>, Acc, State) ->
    {noreply, State#{buffer => Acc}}.

parse_query(<<"\r\n", Rest/bytes>>, Acc, State) ->
    search(State#{query => Acc, buffer => Rest});
parse_query(<<C, Rest/bytes>>, Acc, State) ->
    parse_query(Rest, <<Acc/bytes, C>>, State);
parse_query(<<>>, Acc, State) ->
    {noreply, State#{buffer => Acc}}.

select(State = #{selector := <<"URL:", Target/bytes>>}) ->
    Data =
        ["<!DOCTYPE html>\n"
         "<html>\n"
         "  <head>\n"
         "    <title>redirect<title/>\n"
         "    <meta http-equiv='refresh' content='1; url=", Target, "' />\n"
         "  </head>\n"
         "  <body>\n"
         "    <a href='", Target, "'>", Target, "</a>\n"
         "  </body>\n"
         "</html>\n"],
    rodent:send(Data, State),
    {stop, normal, State};
select(State = #{selector := <<>>}) ->
    select(State#{selector := <<"/">>});
select(State = #{selector := <<"/">>}) ->
    {ok, App} = application:get_application(),
    {ok, Index} = application:get_env(App, index),
    PrivDir = code:priv_dir(App),
    File = filename:join(PrivDir, Index),
    Data = format_file(File, State),
    rodent:send([Data, ".\r\n"], State),
    {stop, normal, State};
select(State = #{selector := Selector, routes := Routes}) ->
    Path = re:split(Selector, "/"),
    case match(Path, Routes) of
        nomatch ->
            Data = rodent:error("Not found", State),
            rodent:send([Data, ".\r\n"], State),
            {stop, normal, State};
        Route ->
            Module = maps:get(callback, Route),
            Options = maps:get(options, Route, undefined),
            call(Module, Options, State#{path => maps:get(path, Route)})
    end.

format_file(File, State) ->
    case file:open(File, [read, binary]) of
        {ok, Device} ->
            format_device(Device, State)
    end.

format_device(Device, State) ->
    case file:read_line(Device) of
        eof ->
            ok = file:close(Device),
            [];
        {ok, Line} ->
            Part = binary:part(Line, 0, byte_size(Line)-1),
            [rodent:format(Part, State), "\r\n" | format_device(Device, State)]
    end.

call(Module, Options, State) ->
    try Module:init(State, Options) of
        {ok, Data} ->
            rodent:send(Data, State),
            {stop, normal, State};
        ok ->
            {stop, normal, State};
        {swap, NewModule, NewOptions} ->
            call(NewModule, NewOptions, State)
    catch Error:Reason:Stacktrace ->
            Data = rodent:error("Internal error", State),
            rodent:send([Data, ".\r\n"], State),
            erlang:raise(Error, Reason, Stacktrace)
    end.


match(Path, [Route|Rest]) ->
    case match_path(maps:get(path, Route), Path) of
        false ->
            match(Path, Rest);
        {true, PathInfo} ->
            Route#{path => PathInfo}
    end;
match(_Path, []) -> nomatch.

match_path([P|Path], [P|Route]) ->
    match_path(Path, Route);
match_path([], PathInfo) -> {true, PathInfo};
match_path(_, _) -> false.


search(State) ->
    Data = rodent:error("Not implemented", State),
    rodent:send([Data, ".\r\n"], State),
    {stop, normal, State}.

access_log(Req = #{selector := Selector, socket := Socket, transport := Transport}) ->
    Query = maps:get(query, Req, <<>>),
    {ok, {Address, Port}} = Transport:peername(Socket),
    {ok, [{send_oct, TX}]} = Transport:getstat(Socket, [send_oct]),
    {ok, [{recv_oct, RX}]} = Transport:getstat(Socket, [recv_oct]),
    logger:notice("~15s:~-5b rx:~b tx:~b ~s ~s",
                  [inet:ntoa(Address), Port, RX, TX, Selector, Query]).
