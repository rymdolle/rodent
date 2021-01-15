-module(rodent_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 5},
    Host = application:get_env(rodent, host, "localhost"),
    Port = application:get_env(rodent, port, 70),
    Options = #{host => Host, port => Port},
    Listener = ranch:child_spec(rodent,
                                ranch_tcp, [{port, Port}],
                                rodent_protocol, Options),
    Procs = [Listener],
    {ok, {SupFlags, Procs}}.

%% internal functions
