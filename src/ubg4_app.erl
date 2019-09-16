%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(ubg4_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

-define(L(Msg), io:format("~b ~p~n", [?LINE, Msg])).
-define(PRIVDIR, code:priv_dir(ubg4)).

%% API.

start(_Type, _Args) ->
    ubg4_data_sup:start_link(),
    ubg4_projector_data_sup:start_link(),

    TemplateCompilationResult = erlydtl:compile_dir(?PRIVDIR ++ "/templates", ubg4_templates),
    ?L(TemplateCompilationResult),

    ubg4_data:read_bible(?PRIVDIR ++ "/pubg-utf8.xml"),

    Routes = [
              {"/projector", ubg4_projector_handler, [{}]},
              {"/navigate", ubg4_navigate_handler, [{}]},
              {"/", ubg4_handler, [{}]},
              {"/:book/:chapter", ubg4_handler, [{}]},
              {"/[...]", ubg4_not_found_handler, [{}]}
             ],

    Dispatch = cowboy_router:compile([{'_', Routes}]),

    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{ env => #{dispatch => Dispatch} }).

stop(_State) ->
    ok.
