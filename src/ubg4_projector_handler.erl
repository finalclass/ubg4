-module(ubg4_projector_handler).

-export([init/2]).

-define(L(Msg), io:format("~b: ~p~n", [?LINE, Msg])).

init(Req, State) ->

    case proplists:get_value(<<"proj_id">>, cowboy_req:parse_qs(Req)) of
        undefined ->
            ResponseBody = ubg4_templates:projector(
                             [
                              {projId, uuid:to_string(uuid:uuid4())}
                             ]),
            ReqWithReply = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                            ResponseBody, Req),
            {ok, ReqWithReply, State};
        ProjId ->
            ReqWithReply2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/text">>},
                                            <<"OK">>, Req),
            {ok, ReqWithReply2, State}
    end.



