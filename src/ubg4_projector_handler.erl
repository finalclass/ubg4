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
                <<"text/plain">> ->
                    %% read body and set the verse. Return the verse 
                    {_, Body, _} = read_body(Req, <<"">>),
                    [ProjId, VerseAddressString] = string:split(Body, ";"),
                    ubg4_projector_data:set_verse(ProjId, VerseAddressString),
                    ReqWithReply = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>},
                                                    <<"ok">>, Req),
                    {ok, ReqWithReply, State};
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
                    {BookName, ChapterNum, VerseNum, VerseText} = ubg4_projector_data:get_verse(ProjId),
                    ResponseBody = lists:flatten(io_lib:format("~s;~p;~p;~s", [[BookName], ChapterNum, VerseNum, [VerseText]])),
                    ReqWithReply2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>},
                                                     ResponseBody, Req),
                    {ok, ReqWithReply2, State}
            end;
        _ -> ubg4_not_found_handler:init(Req, State)
    end.
    



