-module(homework6).
-behaviour(application).

%% API
-export([start/0, start/2, stop/1, create/1, insert/2, insert/3, lookup/2]).

%% Callbacks
-export([start/2, stop/1]).

start() ->
    application:start(cache_supervisor),
    {ok, self()}.

start(_Type, _Args) ->
    application:start(cache_supervisor),
    {ok, self()}.

stop(_State) ->
    ok.

create(TableName) ->
    cache_server:start_link(TableName).

insert(TableName, Key, Value) ->
    cache_server:insert(TableName, Key, Value).

insert(TableName, Key, Value, Expiry) ->
    cache_server:insert(TableName, Key, Value, Expiry).

lookup(TableName, Key) ->
    cache_server:lookup(TableName, Key).
