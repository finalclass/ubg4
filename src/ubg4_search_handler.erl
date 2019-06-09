-module(ubg4_search_handler).

-export([init/2]).

-define(L(Msg), io:format("~b: ~p~n", [?LINE, Msg])).

init(Req, State) ->
    case proplists:get_value(<<"term">>, cowboy_req:parse_qs(Req)) of
        undefined ->
            ResponseBody = ubg4_templates:search([]),
            ReqWithReply = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                            ResponseBody, Req),
            {ok, ReqWithReply, State};
        Term ->
            {EncodedBookName, ChapterNum, VerseNum, _} = ubg4_data:find_verse(Term),
            Location = lists:concat([
                                     "/", binary_to_list(EncodedBookName),
                                     "/", integer_to_list(ChapterNum),
                                     "#", integer_to_list(VerseNum)
                                    ]),
            ReqWithRedirect = cowboy_req:reply(302, #{<<"Location">> => Location}, <<>>, Req),
            {ok, ReqWithRedirect, State}
    end.
    



