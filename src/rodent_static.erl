-module(rodent_static).

-include_lib("kernel/include/file.hrl").

-export([init/2]).

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
    case file:list_dir(Dir) of
        {ok, Files} ->
            {ok, [begin
                      Selector = filename:join(maps:get(selector, Req), File),
                      case file:read_file_info(filename:join(Dir, File)) of
                          {ok, #file_info{type = regular}} ->
                              rodent:file(File, Selector, Req);
                          {ok, #file_info{type = directory}} ->
                              rodent:menu(File, Selector, Req)
                      end
                  end || File <- Files]};
        {error, enoent} ->
            {ok, rodent:error("Not found", Req)}
    end;
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
            init(Req, {dir, File});
        {error, enoent} ->
            {ok, rodent:error("Not found", Req)}
    end.
