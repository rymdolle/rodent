-module(rodent_static).

-include_lib("kernel/include/file.hrl").

-export([init/2]).

init(Req = #{selector := <<"URL:",Link/bytes>>}, _) ->
    Data = ["<!DOCTYPE html>\n",
            "<html><head>\n",
            "<meta http-equiv='refresh' content='5;URL=", Link, "' />\n",
            "</head>\n",
            "<body><a href='", Link, "'>", Link, "</a>\n",
            "</body></html>\n"
           ],
    {ok, Data, Req};
init(Req, {priv_file, Application, Name}) ->
    Dir = code:priv_dir(Application),
    File = filename:join(Dir, Name),
    case file:read_file_info(File) of
        {ok, #file_info{type = regular, mtime = MTime}} ->
            Time = io_lib:format("Last edit: ~p", [MTime]),
            GitHub = rodent:url("github.com/rymdolle", "https://github.com/rymdolle", Req),
            {ok, format_file(File, Req) ++ [GitHub, rodent:info(Time, Req)], Req}
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
