# Snowflake: a 64-bit UUID generator #

Original design by [Twitter](https://github.com/twitter/snowflake).

```erlang
1> application:start(snowflake).
2> snowflake:new().
<<0,231,97,68,12,192,0,0>>
3> snowflake:serialize(snowflake:new()).
<<"AOdiTg5AAAA=">>
4> sets:size(sets:from_list([snowflake:new() || _I <- lists:seq(1,100000)])).
100000
5> lists:map(fun ({T, _}) -> T end,[timer:tc(snowflake, new, []) || _I <- lists:seq(1,100)]).
[93,46,42,42,42,53,55,43,42,43,73,36,34,33,39,52,48,39,35,
 54,36,34,33,34,40,35,33,33,34|...]
```
