% @author Joseph Abrahamson <me@jspha.com>
%% @copyright 2012 Joseph Abrahamson

%% @doc Snowflake, a distributed Erlang 64bit UUID server. Based on
%% the Twitter project of the same name.

-module(snowflake).
-author('Joseph Abrahamson <me@jspha.com>').

%% Public
-export([new/0, new/1]).

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).

%% The supervisor name is bootstraped from this module.
-define(SUP, snowstorm_sup).

%% ----------
%% Some types

-type uuid() :: <<_:64>>.
%% A uuid binary consisting of `<<Time, MachineID, SequenceID>>' where
%% `Time' is a 42 bit binary integer recording milliseconds since UTC
%% 2012-01-01T00:00:00Z, `MachineID' a 10 bit integer recording the
%% snowflake machine which generated the said UUID, and `SequenceID'
%% is a 12 bit integer counting the number of UUIDs generated on this
%% server, this millisecond.


%% ----------
%% Public API

-spec
%% @doc Creates a new snowflake.
%% @equiv new(normal)
new() -> uuid().
new() -> new(normal).    

-spec
%% @doc Creates a new snowflake in a named series. Two snowflakes with
%% different names can be identical, uniqueness is only guaranteed
%% within a named sequence (and up to 4096
%% snowflakes/millisecond/machine).
new(Name :: atom() | pid()) -> uuid().
new(Name) when is_atom(Name) ->     
    Children = supervisor:which_children(?SUP),
    case lists:keyfind(Name, 1, Children) of
	false -> 
	    new(start_snowstorm(Name));
	{Name, undefined, _, _} ->
	    error_logger:info_msg(
	      "Found a dead snowstorm, recreating it."),
	    _ = supervisor:delete_child(?SUP, Name),
	    new(start_snowstorm(Name));
	{Name, restarting, _, _} ->
	    error_logger:info_msg(
	      "Tried to get a flake from a restarting snowstorm!"),
	    timer:sleep(500),
	    new(Name);
	{Name, Pid, _, _} -> new(Pid)
    end;
new(Name) when is_pid(Name) ->
    gen_server:call(Name, new).
					 

%% -----------
%% Private API

start_snowstorm(Name) when is_atom(Name) ->
    {ok, Storm} = 
	supervisor:start_child(
	  ?SUP,
	  {Name, {sf_snowstorm, start_link, [Name]},
	   permanent, 5000, worker, [sf_snowstorm]}),
    Storm.

%% ---------------------
%% Application Behaviour

start(_Type, _Args) ->
    supervisor:start_link({local, ?SUP}, ?MODULE, []).

stop(_State) ->
    ok.


%% --------------------
%% Supervisor behaviour

%% Starts a blank supervisor --- real children will be added
%% dynamically.

init(_Args) ->
    {ok, {{one_for_one, 5, 10}, []}}.

