-module(ubg4_handler).

-export([init/2]).

-define(L(Msg), io:format("~b: ~p~n", [?LINE, Msg])).

init(Req, State) ->
    Books = ubg4_data:get_books(),

    BookEncodedName = cowboy_req:binding(book, Req, <<"rdz">>),
    ChapterNumber = cowboy_req:binding(chapter, Req, <<"1">>),
    ShowChapters = proplists:get_value(<<"chapters">>, cowboy_req:parse_qs(Req)),

    case ubg4_data:get_chapter(BookEncodedName, ChapterNumber) of
        {error, _} ->
            ubg4_not_found_handler:init(Req, State);
        Chapter ->
            CurrentChapter = maps:get(number, Chapter),
            NofChapters = maps:get(book_nof_chapters, Chapter),
            PrevChapterNumber = case CurrentChapter > 1 of
                              true -> CurrentChapter - 1;
                              false -> null
                          end,
            NextChapterNumber = case CurrentChapter < NofChapters - 1 of
                              true -> CurrentChapter + 1;
                              false -> null
                          end,
            ResponseBody = ubg4_templates:index(
              [
               {books, Books},
               {chapter, Chapter},
               {showChapters, ShowChapters},
               {nofChapters, NofChapters},
               {prevChapterNumber, PrevChapterNumber},
               {nextChapterNumber, NextChapterNumber}
              ]),

            ReqWithReply = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                            ResponseBody, Req),
            {ok, ReqWithReply, State}
    end.

