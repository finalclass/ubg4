-module(ubg4_random_verse_handler).

-export([init/2]).

-define(L(Msg), io:format("~b: ~p~n", [?LINE, Msg])).

init(Req, State) ->
    Verse = ubg4_data:get_random_verse(),

    Books = list_to_binary(
              string:join(
                lists:map(fun (Item) -> binary_to_list(<<"\"", Item/binary, "\"">>) end, maps:get(all_books, Verse)), ",")),

    Book = maps:get(book, Verse),
    ChapterNumber = list_to_binary(integer_to_list(maps:get(chapter_number, Verse))),
    VerseNumber = list_to_binary(integer_to_list(maps:get(verse_number, Verse))),
    VerseText = maps:get(verse_text, Verse),
    MaxChapters = list_to_binary(integer_to_list(maps:get(max_chapters, Verse))),
    MaxVerse = list_to_binary(integer_to_list(maps:get(max_verses, Verse))),

     ResponseBody = <<"{",
                      "\"book\":",  "\"",  Book/binary, "\",",
                      "\"chapter_number\":", ChapterNumber/binary, ",",
                      "\"verse_number\":", VerseNumber/binary, ","
                      "\"verse_text\":\"", VerseText/binary, "\",",
                      "\"all_books\":[", Books/binary, "],",
                      "\"max_chapters\":", MaxChapters/binary, ",",
                      "\"max_verses\":", MaxVerse/binary,
                      "}">>,
    
    ReqWithReply = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
                                    ResponseBody, Req),
    {ok, ReqWithReply, State}.

