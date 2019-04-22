-module(ubg4_handler).

-export([init/2]).

-define(L(Msg), io:format("~b: ~p~n", [?LINE, Msg])).

init(Req, State) ->
    Books = ubg4_data:get_books(),

    BookEncodedName = cowboy_req:binding(book, Req, <<"rdz">>),
    ChapterNumber = cowboy_req:binding(chapter, Req, <<"1">>),

    case ubg4_data:get_chapter(BookEncodedName, ChapterNumber) of
        {error, _} ->
            ubg4_not_found_handler:init(Req, State);
        Chapter ->
            NoLayout = proplists:get_value(<<"noLayout">>, cowboy_req:parse_qs(Req), false),
            ResponseBody = case NoLayout of 
                               <<"true">> ->
                                   ResponseBody = ubg4_templates:chapter([{chapter, Chapter}]);
                               _ -> 
                                   ResponseBody = ubg4_templates:index(
                                                    [
                                                     {books, Books},
                                                     {chapter, Chapter}
                                                    ])
                   end,

            ReqWithReply = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                            ResponseBody, Req),
            {ok, ReqWithReply, State}
    end.

