-module(cache_server).
-behaviour(gen_server).

%% API
-export([start_link/1, insert/3, insert/4, lookup/2]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {table, cleanup_timer}).

start_link(TableName) ->
    gen_server:start_link({local, TableName}, ?MODULE, TableName, []).

init(TableName) ->
    Table = ets:new(TableName, [named_table, public]),
    Timer = erlang:send_after(60000, self(), cleanup),
    {ok, #state{table = Table, cleanup_timer = Timer}}.

insert(TableName, Key, Value) ->
    gen_server:cast(TableName, {insert, Key, Value}).

insert(TableName, Key, Value, Expiry) ->
    gen_server:cast(TableName, {insert_with_expiry, Key, Value, Expiry}).

lookup(TableName, Key) ->
    gen_server:call(TableName, {lookup, Key}).

handle_cast({insert, Key, Value}, State) ->
    ets:insert(State#state.table, {Key, Value, undefined}),
    {noreply, State};

handle_cast({insert_with_expiry, Key, Value, Expiry}, State) ->
    ExpiryTime = erlang:system_time(seconds) + Expiry,
    ets:insert(State#state.table, {Key, Value, ExpiryTime}),
    {noreply, State}.

handle_call({lookup, Key}, _From, State) ->
    case ets:lookup(State#state.table, Key) of
        [{Key, Value, Expiry}] when (Expiry == undefined; Expiry > erlang:system_time(seconds)) ->
            {reply, Value, State};
        _ ->
            {reply, undefined, State}
    end.

handle_info(cleanup, State) ->
    now = erlang:system_time(seconds),
    ets:select(State#state.table, [{{'$_', '$1', '$2'}, [{'<', '$2', now}], ['$_']}]),
    Timer = erlang:send_after(60000, self(), cleanup),
    {noreply, State#state{cleanup_timer = Timer}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
