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

-module(rodent).

-export([start/0]).
-export([format/2]).
-export([info/2, file/3, menu/3, bin/3, url/3, error/2]).
-export([send/2]).

start() ->
    application:ensure_all_started(rodent).

format(<<>>, _State) -> [];
format(<<"%%", Rest/bytes>>, State) ->
    [$% | format(Rest, State)];
format(<<"%h", Rest/bytes>>, State) ->
    [maps:get(host, State)|format(Rest, State)];
format(<<"%p", Rest/bytes>>, State) ->
    Port = maps:get(port, State),
    [integer_to_list(Port)|format(Rest, State)];
format(<<C,Rest/bytes>>, State) ->
    [C|format(Rest, State)].


encode(Type, Description, Selector, Host, Port) ->
    io_lib:format("~c~s\t~s\t~s\t~b\r\n", [Type, Description, Selector, Host, Port]).

info(Message, #{host := Host, port := Port}) ->
    encode($i, Message, "", Host, Port).

file(Name, Selector, #{host := Host, port := Port}) ->
    encode($0, Name, Selector, Host, Port).

menu(Name, Selector, #{host := Host, port := Port}) ->
    encode($1, Name, Selector, Host, Port).

bin(Name, Selector, #{host := Host, port := Port}) ->
    encode($9, Name, Selector, Host, Port).

url(Name, Target, #{host := Host, port := Port}) ->
    encode($h, Name, ["URL:",Target], Host, Port).

error(Message, #{host := Host, port := Port}) ->
    io_lib:format("3~s\t\t~s\t~b\r\n.\r\n", [Message, Host, Port]).

send({sendfile, Offset, Size, File}, #{socket := Socket, transport := Transport}) ->
    Transport:sendfile(Socket, File, Offset, Size);
send({sendfile, File}, #{socket := Socket, transport := Transport}) ->
    Transport:sendfile(Socket, File);
send(Data, #{socket := Socket, transport := Transport}) ->
    Transport:send(Socket, Data).
