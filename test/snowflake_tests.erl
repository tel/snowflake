-module(snowflake_tests).

-include_lib("eunit/include/eunit.hrl").

collision_testGen() ->
    ?_assertEqual(
       false,
       lists:any(
	 fun ({A, B}) -> A =:= B end,
	 [{snowflake:new(), snowflake:new()} || _I <- lists:seq(1,40)])).

collision_test_() ->
    application:start(snowflake),
    [collision_testGen() || _I <- lists:seq(1,200)].
