-module(rodent_static).

-include_lib("kernel/include/file.hrl").

-export([init/2]).

init(Req, {priv_file, Application, Name}) ->
    Dir = code:priv_dir(Application),
    File = filename:join(Dir, Name),
    case file:read_file_info(File) of
        {ok, #file_info{type = regular, size = Size}} ->
            {ok, {sendfile, 0, Size, File}, Req}
    end.
