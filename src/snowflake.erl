% @author Joseph Abrahamson <me@jspha.com>
%% @copyright 2012 Joseph Abrahamson

%% @doc Snowflake, a distributed Erlang 64bit UUID server. Based on
%% the Twitter project of the same name.

-module(snowflake).
-author('Joseph Abrahamson <me@jspha.com>').

%% Public
-export([new/0, new/1]).
-export_type([uuid/0]).

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
%% @doc Generates a new snowflake from the specified storm. The Storm can
%% be specified by name (`atom()') or PID.
new(Storm :: atom() | pid()) -> uuid().
new(Name) when is_atom(Name) ->
    case find_nearest() of
	Server ->
	    new(Server, Name)
    end;
new(Storm) when is_pid(Storm) ->
    gen_server:call(Storm, new).

-spec
%% @doc Creates a new snowflake in a named series. Two snowflakes with
%% different names can be identical, uniqueness is only guaranteed
%% within a named sequence (and up to 4096
%% snowflakes/millisecond/machine).
new(Server :: pid(), Name :: atom() | pid()) -> uuid().
new(Server, Name) when is_atom(Name) ->     
    Children = supervisor:which_children(Server),
    case lists:keyfind(Name, 1, Children) of
	false -> 
	    new(start_snowstorm(Server, Name));
	{Name, undefined, _, _} ->
	    error_logger:info_msg(
	      "Found a dead snowstorm, recreating it."),
	    _ = supervisor:delete_child(Server, Name),
	    new(start_snowstorm(Server, Name));
	{Name, restarting, _, _} ->
	    error_logger:info_msg(
	      "Caught a restarting snowstorm. Waiting and trying again."),
	    timer:sleep(500),
	    new(Name);
	{Name, Pid, _, _} -> new(Pid)
    end.


-spec
serialize(uuid()) -> binary().
serialize(UUID) -> base64:encode(UUID).

-spec
deserialize(binary() | nonempty_string()) -> uuid().
deserialize(Bin) when is_binary(Bin) -> base64:decode(Bin);
deserialize(Str) when is_list(Str) -> base64:decode(list_to_binary(Str)).

%% -----------
%% Private API

-spec
%% @doc Autodiscovery mechanism that searches first for a snowflake
%% service on this node, and then picks one randomly from all known.
find_nearest() -> pid() | none.
find_nearest() ->
    case pg2:get_closest_pid(?MODULE) of
	{error, _} -> none;
	Pid -> Pid
    end.
	     
-spec
start_snowstorm(Server :: pid(), Name :: atom()) -> Pid :: pid().
start_snowstorm(Server, Name) ->
    {ok, Storm} = 
	supervisor:start_child(
	  Server,
	  {Name, {sf_snowstorm, start_link, [Name]},
	   permanent, 5000, worker, [sf_snowstorm]}),
    Storm.

%% ---------------------
%% Application Behaviour

start(_Type, _Args) ->
    {ok, Pid} = supervisor:start_link({local, ?SUP}, ?MODULE, []),
    pg2:create(?MODULE),
    ok = pg2:join(?MODULE, Pid),
    {ok, Pid}.

stop(_State) ->
    case whereis(?SUP) of
	undefined -> ok;
	Pid ->
	    _ = pg2:leave(?MODULE, Pid),
	    ok
    end.


%% --------------------
%% Supervisor behaviour

%% Starts a blank supervisor --- real children will be added
%% dynamically.

init(_Args) ->
    {ok, {{one_for_one, 5, 10}, []}}.

