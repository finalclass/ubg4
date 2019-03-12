-module(ubg4_data).

-export([read_bible/1, read_bible/0]).
-export([test/0]).
-export([get_chapter/3]).

-define(L(Msg), io:format("~b: ~p~n", [?LINE, Msg])).

read_bible() ->
    read_bible("./priv/pubg-utf8.xml").

read_bible(Path) ->
    Xml = erlang:element(1, xmerl_scan:file(Path)),
    Books = get_books(Xml),
    
    #{
      xml => Xml,
      books => Books
     }.

get_books(BibleXml) ->
    BNodes = xmerl_xpath:string("/bible/b", BibleXml),
    lists:map(
      fun(BookNode) ->
              Chapters = get_chapters(BookNode),

              #{
                full_name => get_bin_attribute("n", BookNode),
                short_name => get_bin_attribute("s", BookNode),
                encoded_name => get_bin_attribute("u", BookNode),
                chapters => Chapters,
                nof_chapters => length(Chapters)
               }
      end,
      BNodes).

get_chapters(BookNode) ->
    ChapterNodes = xmerl_xpath:string("c", BookNode),
    lists:map(
      fun(ChapterNode) ->
              VNodes = xmerl_xpath:string("v", ChapterNode),
              Verses = lists:map(
                         fun(Verse) ->
                                 #{
                                   number => get_bin_attribute("n", Verse),
                                   text => get_text(Verse)
                                  }
                         end,
                         VNodes),

              #{
                book_name => get_bin_attribute("n", BookNode),
                number => get_bin_attribute("n", ChapterNode),
                verses => Verses
               }
      end, 
      ChapterNodes).

get_chapter(BookEncodedName, ChapterNumberBin, Bible) ->
    [Book|_] = lists:filter(fun(B) -> maps:get(encoded_name, B) == BookEncodedName end, maps:get(books, Bible)),
    ChapterNumber = list_to_integer(binary_to_list(ChapterNumberBin)),
    lists:nth(ChapterNumber, maps:get(chapters, Book)).

test() ->
    get_chapter(<<"lk">>, <<"1">>, read_bible()).

%% PRIV

get_text(XmlElement) ->
    unicode:characters_to_binary(
      erlang:element(
        3, xmerl_xpath:string("string(text())", XmlElement))).

get_bin_attribute(AttrName, XmlElement) ->
  unicode:characters_to_binary(
    erlang:element(
      3, xmerl_xpath:string("string(@" ++ AttrName ++ ")", XmlElement))).
