-module(rodent_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, App} = application:get_application(),
    Host = application:get_env(App, host, "localhost"),
    Port = application:get_env(App, port, 70),
    Routes = [Route#{path => re:split(maps:get(path, Route), "/")} ||
                 Route <- application:get_env(App, routes, [])],
    Options = #{host => Host, port => Port, routes => Routes},
    Listener = ranch:child_spec(App,
                                ranch_tcp, [{port, Port}],
                                rodent_protocol, Options),
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 5},
    Procs = [Listener],
    {ok, {SupFlags, Procs}}.

%% internal functions
