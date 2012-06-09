-module(snowflake_tests).

-include_lib("eunit/include/eunit.hrl").

collision_test_() ->
    snowflake:start(),
    Snowflakes = 
	[{snowflake:new(), snowflake:new()} || I <- lists:seq(1,20)],
    [?_assertNot(A =:= B) || {A, B} <- Snowflakes].
