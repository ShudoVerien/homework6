-module(cache_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, cache_supervisor}, ?MODULE, []).

init([]) ->
    Children = [
        {cache_server, {cache_server, start_link, ["cache_table"]}, permanent, 5000, worker, [cache_server]}
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.
