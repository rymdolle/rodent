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

-module(rodent_static).

-include_lib("kernel/include/file.hrl").

-export([init/2]).

init(_Req, {data, Data}) ->
    {ok, Data};
init(_Req, {format, File}) ->
    {ok, {format, File}};
init(Req, {file, File}) ->
    case file:read_file_info(File) of
        {ok, #file_info{type = regular, size = Size}} ->
            {ok, {sendfile, 0, Size, File}};
        {error, enoent} ->
            {ok, rodent:error("Not found", Req)}
    end;
init(Req, {priv_file, Application, File}) ->
    Dir = code:priv_dir(Application),
    init(Req, {file, filename:join(Dir, File)});
init(Req, {dir, Dir}) ->
    File = filename:join([Dir|maps:get(path_info, Req)]),
    case file:read_file_info(File) of
        {ok, #file_info{type = directory}} ->
            {ok, Files} = file:list_dir(File),
            Fun = fun(F, Acc) ->
                          Selector = filename:join(maps:get(selector, Req), F),
                          case file:read_file_info(filename:join(File, F)) of
                              {ok, #file_info{type = regular}} ->
                                  [rodent:file(F, Selector, Req)|Acc];
                              {ok, #file_info{type = directory}} ->
                                  [rodent:menu(F, Selector, Req)|Acc];
                              _ ->
                                  Acc
                          end
                  end,
            {ok, lists:foldr(Fun, [], Files)};
        {ok, #file_info{type = regular, size = Size}} ->
            {ok, {sendfile, 0, Size, File}};
        {error, enoent} ->
            {ok, rodent:error("Not found", Req)}
    end;
init(Req, priv_dir) ->
    {ok, Application} = application:get_application(),
    init(Req, {priv_dir, Application});
init(Req, {priv_dir, Application}) ->
    init(Req, {priv_dir, Application, ""});
init(Req = #{path_info := []}, {priv_dir, Application, Dir}) ->
    PrivDir = code:priv_dir(Application),
    init(Req, {dir, filename:join(PrivDir, Dir)});
init(Req = #{path_info := Path}, {priv_dir, Application, Dir}) ->
    PrivDir = code:priv_dir(Application),
    Target = filename:join(Path),
    File = filename:join([PrivDir, Dir, Target]),
    case file:read_file_info(File) of
        {ok, #file_info{type = regular, size = Size}} ->
            {ok, {sendfile, 0, Size, File}};
        {ok, #file_info{type = directory}} ->
            init(Req, {dir, File});
        {error, enoent} ->
            {ok, rodent:error("Not found", Req)}
    end.
