-module(rodent_git).

-export([init/2]).
-export([tree/2]).

init(Req = #{path := []}, Dir) ->
    init(Req#{path := [<<>>]}, Dir);
init(Req = #{path := [<<>>]}, _Dir) ->
    {ok, rodent:error("Not found", Req)};
init(Req = #{path := [Name]}, Dir) ->
    init(Req#{path => [Name, "master^{tree}"]}, Dir);
init(Req = #{path := [Name, Id]}, Dir) ->
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
    Base = io_lib:format("git -C ~s cat-file", [Repo]),
    case os:cmd(io_lib:format("~s -t ~s", [Base, Id])) of
        "tree" ++ _ ->
            Data = os:cmd(io_lib:format("~s -p ~s", [Base, Id])),
            {menu, [begin
                        [Info, File] = string:lexemes(Line, "\t"),
                        [Permission, Type, Obj] = string:lexemes(Info, " "),
                        {Repo, File, Type, Obj, Permission}
                    end || Line <- re:split(Data, <<"\n">>), byte_size(Line) > 0]};
        "blob" ++ _ ->
            {file, os:cmd(io_lib:format("~s -p ~s", [Base, Id]))};
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
