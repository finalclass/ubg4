-module(ubg4_projector_handler).

-export([init/2]).

-define(L(Msg), io:format("~b: ~p~n", [?LINE, Msg])).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            case cowboy_req:header(<<"content-type">>, Req) of
                "text/plain" ->
                    %% read body and set the verse. Return the verse 
                    Body = read_body(Req, <<"">>),
                    ?L(Body);
                _ -> ubg4_not_found_handler:init(Req, State)
            end;
        <<"GET">> ->
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
                    Verse = ubg4_projector_data:get_verse(ProjId),
                    ReqWithReply2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>},
                                                     Verse, Req),
                    {ok, ReqWithReply2, State}
            end;
        _ -> ubg4_not_found_handler:init(Req, State)
    end.
    



