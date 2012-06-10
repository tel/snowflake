% @author Joseph Abrahamson <me@jspha.com>
%% @copyright 2012 Joseph Abrahamson

%% @doc Snowflake, a distributed Erlang 64bit UUID server.

-module(snowflake).

-author('Joseph Abrahamson <me@jspha.com>').

-behaviour(gen_server).

%% Public API
-export([new/0, new/1]).
% -export([request/0, request/1, await/0, await/1]).
-export([start_link/0, start/0]).

%% Behaviour API
-export([init/1, terminate/2, handle_info/2, code_change/3]).
-export([handle_call/3, handle_cast/2]).

%% Private API
-export([send_uuid/1, send_uuid/3]).

-define(SNOWFLAKE_EPOCH,
	calendar:datetime_to_gregorian_seconds({{2012, 1, 1}, {0,0,0}})).
-define(STD_EPOCH,
	calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})).
-define(MS_EPOCH_DIFF, 1000*(?SNOWFLAKE_EPOCH - ?STD_EPOCH)).

% -record(snowflake_state, 
% 	{now :: <<_:41>>,
% 	 machine :: <<_:10>>,
% 	 sequence :: <<_:12>>}).

% -type snowflake_state() :: #snowflake_state{}.

%% --------------
%% Initialization

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% ----------
%% Public API

-type uuid() :: <<_:64>>.
%% A uuid binary consisting of `<<Time, MachineID, SequenceID>>' where
%% `Time' is a 42 bit binary integer recording milliseconds since UTC
%% 2012-01-01T00:00:00Z, `MachineID' a 10 bit integer recording the
%% snowflake machine which generated the said UUID, and `SequenceID'
%% is a 12 bit integer counting the number of UUIDs generated on this
%% server, this millisecond.

-spec new() -> uuid().
%% @equiv new(default).
new() ->
    new(default).

-spec new(Class :: atom()) -> uuid().
%% @doc Synchronously returns a new snowflake `uuid()'.
new(Class) ->
    gen_server:call(?MODULE, {new, Class}).

% -spec request() -> ok.
% %% @equiv request(default).
% request() ->
%     request(default).

% % -spec request(Class :: atom()) -> ok.
% %% @doc Request a new `uuid()' from the server.
% request(Class) ->
%     gen_server:call(?MODULE, {request, Class}).

% -spec await() -> uuid().
% %% @equiv await(infinity).
% await() ->
%     await(infinity).

% -spec await(Timeout :: integer() | infinity) -> {ok, uuid()} | timeout.
% %% @doc Wait for a previously requested `uuid()'. Will wait for
% %% `Timeout' milliseconds.
% await(Timeout) ->
%     receive 
% 	{reply, uuid, UUID} -> {ok, UUID}
%     after Timeout ->
% 	    timeout
%     end.
    

%% -----------------
%% Callback handling

send_uuid(From) ->
    send_uuid(From, 0, 0).

send_uuid(From, MID, SID) ->
    Now = snowflake_now(),
    UUID = <<Now:42, MID:10, SID:12>>,
    gen_server:reply(From, UUID).

handle_call({new, Class}, From, MID) ->
    %% TODO
    %% Here we can use the current state to update the sequence counter
    %% for any particular `Class' of ID.
    %% As a shortcut, let's just pick a random number between 0 and
    %% 2^12-1
    erlang:spawn_link(?MODULE, send_uuid, 
		      [From, MID, random:uniform(trunc(math:pow(2,12)-1))]),
    {noreply, MID}.

handle_cast(_Message, State) ->
    {noreply, State}.

%% ----------------
%% Server framework

init(_Args) ->
    case application:get_env(machine_id) of
	undefined -> {ok, 0};
	{ok, Number} -> {ok, Number}
    end.

terminate(normal, _State) ->
    ok.

handle_info(_Message, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------
%% Utilities

%% @doc returns the number of milliseconds since UTC January 1st,
%% 2012.
snowflake_now() ->
    {MegS, S, MuS} = erlang:now(),
    Secs = (1000000*MegS + S)*1000 + trunc(MuS/1000),
    Secs - ?MS_EPOCH_DIFF.
