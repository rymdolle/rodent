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

terminate(_Reason, #{}) ->
    ok.


%%% Internal functions

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


select(State) ->
    case rodent_static:init(State, {priv_file, rodent, "index.txt"}) of
        {ok, Data, Req} ->
            rodent:send(Data, Req),
            {stop, normal, Req}
    end.

search(State) ->
    Data = rodent:info("Not implemented", State),
    rodent:send(Data, State),
    {stop, normal, State}.
