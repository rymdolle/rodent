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
-export([encode/4, encode/5]).
-export([info/2, file/3, menu/3, bin/3, url/3, error/2]).
-export([send/2]).

start() ->
    application:ensure_all_started(rodent).

format(<<>>, _State) -> [];
format(<<"%%", Rest/bytes>>, State) ->
    [$% | format(Rest, State)];
format(<<"%host%", Rest/bytes>>, State) ->
    [maps:get(host, State)|format(Rest, State)];
format(<<"%port%", Rest/bytes>>, State) ->
    Port = maps:get(port, State),
    [integer_to_list(Port)|format(Rest, State)];
format(<<"%time%", Rest/bytes>>, State) ->
    Time = erlang:universaltime(),
    [io_lib:format("~p", [Time]) | format(Rest, State)];
format(<<"%version%", Rest/bytes>>, State) ->
    {ok, Version} = application:get_key(rodent, vsn),
    [Version | format(Rest, State)];
format(<<"%application%", Rest/bytes>>, State) ->
    {ok, Application} = application:get_application(rodent),
    [atom_to_binary(Application) | format(Rest, State)];
format(<<C,Rest/bytes>>, State) ->
    [C|format(Rest, State)].


encode(Type, Description, Selector, #{host := Host, port := Port}) ->
    encode(Type, Description, Selector, Host, Port).
encode(Type, Description, Selector, Host, Port) ->
    io_lib:format("~c~s\t~s\t~s\t~b\r\n", [Type, Description, Selector, Host, Port]).

info(Message, State) ->
    encode($i, Message, "", State).

file(Name, Selector, State) ->
    encode($0, Name, Selector, State).

menu(Name, Selector, State) ->
    encode($1, Name, Selector, State).

bin(Name, Selector, State) ->
    encode($9, Name, Selector, State).

url(Name, Target, State) ->
    encode($h, Name, ["URL:",Target], State).

error(Message, State) ->
    encode($3, Message, "", State).

send({format, {data, Format}}, State) ->
    Data = format(Format, State),
    send(Data, State);
send({format, {priv_file, Application, File}}, State) ->
    Dir = code:priv_dir(Application),
    send({format, {file, filename:join(Dir, File)}}, State);
send({format, {file, File}}, State) ->
    case file:open(File, [read, binary]) of
        {ok, Device} ->
            format_send(Device, State)
    end;
send({sendfile, Offset, Size, File}, #{socket := Socket, transport := Transport}) ->
    Transport:sendfile(Socket, File, Offset, Size);
send({sendfile, File}, #{socket := Socket, transport := Transport}) ->
    Transport:sendfile(Socket, File);
send(Data, #{socket := Socket, transport := Transport}) ->
    Transport:send(Socket, Data).

format_send(Device, State) ->
    case file:read_line(Device) of
        eof ->
            file:close(Device),
            send(".\r\n", State);
        {ok, Line} ->
            Part = binary:part(Line, 0, byte_size(Line)-1),
            Data = [format(Part, State), "\r\n"],
            send(Data, State),
            format_send(Device, State)
    end.
