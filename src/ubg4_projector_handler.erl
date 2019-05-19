-module(ubg4_projector_handler).

-export([init/2]).

-define(L(Msg), io:format("~b: ~p~n", [?LINE, Msg])).

init(Req, State) ->
    ?L("OK"),
    ResponseBody = ubg4_templates:projector(
                     [
                     ]),
    ReqWithReply = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                    ResponseBody, Req),
    {ok, ReqWithReply, State}.

