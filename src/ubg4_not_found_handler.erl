-module(ubg4_not_found_handler).

-export([init/2]).

-define(L(Msg), io:format("~b: ~p~n", [?LINE, Msg])).

init(Req, State) ->
    ResponseBody = ubg4_templates:not_found([]),

    ReqWithReply = cowboy_req:reply(404, #{<<"content-type">> => <<"text/html">>},
                           ResponseBody, Req),

    {ok, ReqWithReply, State}.

