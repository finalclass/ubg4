%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(ubg4_handler).

-export([init/2]).

-define(L(Msg), io:format("~b ~p~n", [?LINE, Msg])).

init(Req0, Opts) ->
    Bible = proplists:get_value(bible, Opts),
    Books = ubg4_data:get_books(Bible),

    {ok, ResponseBody} = ubg4_templates_index:render(
                           [
                            {books, Books}
                           ]),

    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                           ResponseBody, Req0),
    {ok, Req, Opts}.

