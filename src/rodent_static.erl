-module(rodent_static).

-include_lib("kernel/include/file.hrl").

-export([init/2]).

init(_Req, {file, File}) ->
    case file:read_file_info(File) of
        {ok, #file_info{type = regular, size = Size}} ->
            {ok, {sendfile, 0, Size, File}}
    end;
init(Req, {priv_file, Application, File}) ->
    Dir = code:priv_dir(Application),
    init(Req, {file, filename:join(Dir, File)});
init(Req, {dir, _Dir}) ->
    %% List directory
    {ok, rodent:error("Not implemented", Req)};
init(Req, {priv_dir, Application}) ->
    init(Req, {priv_dir, Application, ""});
init(Req = #{path := []}, {priv_dir, Application, Dir}) ->
    PrivDir = code:priv_dir(Application),
    init(Req, {dir, filename:join(PrivDir, Dir)});
init(Req = #{path := Path}, {priv_dir, Application, Dir}) ->
    PrivDir = code:priv_dir(Application),
    Target = filename:join(Path),
    File = filename:join([PrivDir, Dir, Target]),
    case file:read_file_info(File) of
        {ok, #file_info{type = regular, size = Size}} ->
            {ok, {sendfile, 0, Size, File}};
        {ok, #file_info{type = directory}} ->
            init(Req, {dir, File})
    end.
