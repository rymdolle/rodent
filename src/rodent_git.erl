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

-module(rodent_git).

-export([init/2]).
-export([tree/2]).

init(Req = #{path_info := []}, Dir) ->
    init(Req#{path_info := [<<>>]}, Dir);
init(Req = #{path_info := [<<>>]}, _Dir) ->
    {ok, rodent:error("Not found", Req)};
init(Req = #{path_info := [Name]}, Dir) ->
    init(Req#{path_info => [Name, <<"master^{tree}">>]}, Dir);
init(Req = #{path_info := [Name, <<"archive.tar">>|_]}, Dir) ->
    Repo = filename:join(Dir, Name),
    case filelib:is_dir(Repo) of
        true ->
            {ok, tar(Repo)};
        false ->
            {ok, rodent:error("Not found", Req)}
    end;
init(Req = #{path_info := [Name, Id]}, Dir) ->
    Repo = filename:join(Dir, Name),
    case filelib:is_dir(Repo) of
        true ->
            case tree(Repo, Id) of
                {file, Data} ->
                    {ok, Data};
                {menu, Tree} ->
                    Format = format_tree(Tree, Req),
                    {ok, [header(Req), Format, ".\r\n"]};
                error ->
                    {ok, rodent:error("Bad selector", Req)}
            end;
        false ->
            {ok, rodent:error("Not found", Req)}
    end.

header(Req) ->
    [rodent:info("         _",    Req),
     rodent:info("       _( )_",  Req),
     rodent:info("   __ (_)  _)", Req),
     rodent:info(" / _  \ | |",   Req),
     rodent:info("( (_) | | |_",  Req),
     rodent:info(" \__  |_)\__)", Req),
     rodent:info("( )_) |",       Req),
     rodent:info(" \___/",        Req),
     rodent:info("",              Req),
     rodent:info("============================================", Req),
     rodent:info("", Req)].

tree(Repo, Id) ->
    case os:cmd(io_lib:format("git -C ~s cat-file -t ~s", [Repo, Id])) of
        "tree" ++ _ ->
            Data = os:cmd(io_lib:format("git -C ~s ls-tree ~s", [Repo, Id])),
            {menu, [begin
                        [Info, File] = string:lexemes(Line, "\t"),
                        [Permission, Type, Obj] = string:lexemes(Info, " "),
                        {Repo, File, Type, Obj, Permission}
                    end || Line <- re:split(Data, <<"\n">>), byte_size(Line) > 0]};
        "blob" ++ _ ->
            {file, os:cmd(io_lib:format("git -C ~s cat-file -p ~s", [Repo, Id]))};
        _ ->
            error
    end.

format_tree([{Repo, File, Type, Id, _Permission}|Rest], State) ->
    Path = io_lib:format("/git/~s/~s", [filename:basename(Repo), Id]),
    case string:trim(Type) of
        <<"tree">> ->
            [rodent:menu(File, Path, State)|format_tree(Rest, State)];
        <<"blob">> ->
            [rodent:file(File, Path, State)|format_tree(Rest, State)]
    end;
format_tree([], _State) -> [].

tar(Repo) ->
    Command = io_lib:format("git -C ~s archive --format tar --prefix ~s/ master",
                            [Repo, filename:rootname(filename:basename(Repo))]),
    os:cmd(Command).
