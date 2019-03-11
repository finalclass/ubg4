%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(ubg4_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

-define(L(Msg), io:format("~b ~p~n", [?LINE, Msg])).

%% API.

start(_Type, _Args) ->
    %% erltl:compile("./priv/index.html"),
    TemplateCompilationResult = erlydtl:compile_file("./priv/index.dtl", ubg4_templates_index),
    Ubg4Data = ubg4_data:read_bible("./priv/pubg-utf8.xml"),
    ?L(TemplateCompilationResult),
    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/", ubg4_handler, [{bible, Ubg4Data}]}
                                            ]}
                                     ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
                                                         env => #{dispatch => Dispatch}
                                                        }),
    ubg4_sup:start_link().

stop(_State) ->
    ok.
