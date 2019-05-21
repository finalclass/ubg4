%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(ubg4_projector_data_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    Restart = permanent,
    Shutdown = infinity,
    Type = worker,

    TaskStrategy = {
                    ubg4_projector_data_id,
                    {ubg4_projector_data, start_link, []},
                    Restart,
                    Shutdown,
                    Type,
                    [ubg4_projector_data]
                   },

    {ok, {{one_for_one, 10, 10}, [TaskStrategy]}}.
