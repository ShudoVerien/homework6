-module(task_2).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).

all() -> [test_insert_and_lookup, test_expiry].

test_insert_and_lookup() ->
    TableName = "cache_test",
    homework6:create(TableName),
    ok = homework6:insert(TableName, "key1", "value1"),
    Value = homework6:lookup(TableName, "key1"),
    ?assertEqual(Value, "value1").

test_expiry() ->
    TableName = "cache_test_expiry",
    homework6:create(TableName),
    ok = homework6:insert(TableName, "key2", "value2", 1),
    timer:sleep(2000), %% Чекаємо більше 1 секунди
    Value = homework6:lookup(TableName, "key2"),
    ?assertEqual(Value, undefined).
