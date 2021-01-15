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
        {ok, #file_info{type = regular}} ->
            {ok, format_file(File, Req), Req}
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
            [rodent:format(Part, State), "\r\n" | format_device(Device, State)]
    end.
