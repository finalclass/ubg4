%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(ubg4_handler).

-export([init/2]).

-define(L(Msg), io:format("~b: ~p~n", [?LINE, Msg])).

init(Req0, State) ->
    Books = ubg4_data:get_books(),
    
    BookEncodedName = cowboy_req:binding(book, Req0, <<"rdz">>),
    ChapterNumber = cowboy_req:binding(chapter, Req0, <<"1">>),

    Chapter = ubg4_data:get_chapter(BookEncodedName, ChapterNumber),

    {ok, ResponseBody} = ubg4_templates_index:render(
                           [
                            {books, Books},
                            {chapter, Chapter}
                           ]),

    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                           ResponseBody, Req0),
    {ok, Req, State}.

