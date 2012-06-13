% @author Joseph Abrahamson <me@jspha.com>
%% @copyright 2012 Joseph Abrahamson

%% @doc Snowstorm, a gen_server which generates snowflakes en masse.

-module(sf_snowstorm).
-author('Joseph Abrahamson <me@jspha.com>').

-behaviour(gen_server).
-export([init/1,
	 terminate/2,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2]).
-define(SERVER, ?MODULE).

-export([start/1, start_link/1]).

%% The snowflake time is milliseconds since the snowflake_epoch,
%% January 1st, 2012.
-define(SNOWFLAKE_EPOCH,
	calendar:datetime_to_gregorian_seconds({{2012, 1, 1}, {0,0,0}})).
-define(STD_EPOCH,
	calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})).
-define(MS_EPOCH_DIFF, 1000*(?SNOWFLAKE_EPOCH - ?STD_EPOCH)).


%% The gen_server state
-record(st, 
	{name :: atom(),
	 last :: integer(),
	 machine :: integer(),
	 count :: integer()}).


%% --------------------
%% Gen_Server callbacks

init([Name]) ->
    {ok, MID} = application:get_env(machine_id),
    {ok, #st{name = Name, last = snowflake_now(),
	     count = 0, machine = MID}}.
    
handle_call(new, _From, State = #st{last = Last, 
				    machine = MID, 
				    count = Count}) ->
    Now = snowflake_now(),
    SID = case Now of
	      Last -> Count;
	      %% New time point, reset the counter.
	      _    -> 0
	  end,
    {reply, 
     <<Now:42, MID:10, SID:12>>, 
     State#st{last = Now, count = SID+1}}.

handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% ------------------
%% Lifecycle dynamics

start(Name) ->
    gen_server:start(?SERVER, [Name], []).

start_link(Name) ->
    gen_server:start_link(?SERVER, [Name], []).


%% ---------
%% Utilities

-spec 
%% @doc returns the number of milliseconds since UTC January 1st,
%% 2012.
snowflake_now() -> integer().
snowflake_now() ->
    {MegS, S, MuS} = erlang:now(),
    Secs = (1000000*MegS + S)*1000 + trunc(MuS/1000),
    Secs - ?MS_EPOCH_DIFF.
