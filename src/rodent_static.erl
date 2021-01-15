-module(rodent_static).

-include_lib("kernel/include/file.hrl").

-export([init/2]).

init(Req, {priv_file, Application, Name}) ->
    Dir = code:priv_dir(Application),
    File = filename:join(Dir, Name),
    case file:read_file_info(File) of
        {ok, #file_info{type = regular, mtime = MTime}} ->
            Time = io_lib:format("Last edit: ~p", [MTime]),
            {ok, format_file(File, Req) ++ [rodent:info(Time, Req)], Req}
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
            [rodent:info(Part, State) | format_device(Device, State)]
    end.
