%%% Copyright (c) 2021 Olle Mattsson <rymdolle@gmail.com>
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

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
handle_info({tcp, Socket, Data}, State = #{buffer := Buffer, socket := Socket})
  when byte_size(Buffer) + byte_size(Data) > 16#4000 ->
    %% Close connection if selector line is longer than 16kB
    {stop, {shutdown, badselector}, State};
handle_info({tcp, Socket, Data}, State = #{buffer := Buffer, socket := Socket}) ->
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
select(State = #{selector := Selector, routes := Routes}) ->
    case match(re:split(Selector, "/"), Routes) of
        nomatch ->
            Data = rodent:error("Not found", State),
            rodent:send(Data, State);
        #{path_info := PathInfo, callback := Callback, args := Args} ->
            call(Callback, Args, State#{path_info => PathInfo})
    end,
    {stop, normal, State}.

call(Module, Options, State) when is_atom(Module) ->
    call(fun Module:init/2, Options, State);
call(Callback, Options, State) when is_function(Callback, 2) ->
    try Callback(State, Options) of
        ok -> ok;
        {ok, Data} ->
            rodent:send(Data, State);
        {swap, NewModule, NewOptions} ->
            call(NewModule, NewOptions, State)
    catch Error:Reason:Stacktrace ->
            Data = rodent:error("Internal error", State),
            rodent:send(Data, State),
            erlang:raise(Error, Reason, Stacktrace)
    end.


match(Selector, [Route|Rest]) ->
    case match_selector(Selector, maps:get(selector, Route)) of
        false ->
            match(Selector, Rest);
        true ->
            Route#{path_info => []};
        {true, PathInfo} ->
            Route#{path_info => PathInfo}
    end;
match(_Selector, []) -> nomatch.

match_selector([Match|Selector], [Match|Route]) ->
    match_selector(Selector, Route);
match_selector([], []) -> true;
match_selector([<<>>], []) -> true; % allow trailing slash
match_selector(Rest, [<<"*">>]) -> {true, Rest};
match_selector(_, _) -> false.


search(State) ->
    Data = rodent:error("Not implemented", State),
    rodent:send(Data, State),
    {stop, normal, State}.

access_log(Req = #{selector := Selector, socket := Socket, transport := Transport}) ->
    Query = maps:get(query, Req, <<>>),
    {ok, {Address, Port}} = Transport:peername(Socket),
    {ok, [{send_oct, TX}]} = Transport:getstat(Socket, [send_oct]),
    {ok, [{recv_oct, RX}]} = Transport:getstat(Socket, [recv_oct]),
    logger:notice("~s:~-5b rx:~b,tx:~b ~s ~s",
                  [inet:ntoa(Address), Port, RX, TX, Selector, Query]).
