-module(ubg4_verse_handler).

-export([init/2]).

-define(L(Msg), io:format("~b: ~p~n", [?LINE, Msg])).

init(Req, State) ->
    BookEncodedName = cowboy_req:binding(book, Req, <<"rdz">>),
    ChapterNum = list_to_integer(binary_to_list(cowboy_req:binding(chapter, Req, <<"1">>))), 
    VerseNum = list_to_integer(binary_to_list(cowboy_req:binding(verse, Req, <<"1">>))),
    
    Verse = ubg4_data:get_verse(BookEncodedName, ChapterNum, VerseNum),
    ResponseBody = element(4, Verse),
    ReqWithReply = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>},
                                    ResponseBody, Req),
    {ok, ReqWithReply, State}.

