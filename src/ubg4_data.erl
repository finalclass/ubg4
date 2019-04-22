-module(ubg4_data).
-behaviour(gen_server).

-export([start_link/0]).
-export([read_bible/1, read_bible/0]).
-export([get_books/0]).
-export([get_chapter/2]).

-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

-define(L(Msg), io:format("~b: ~p~n", [?LINE, Msg])).
-define(PRIVDIR, code:priv_dir(ubg4)).

%% ---------------------------
%% API
%% ---------------------------

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

read_bible(Path) ->
    gen_server:call({global, ?MODULE}, {read_bible, Path}).

read_bible() ->
    read_bible(?PRIVDIR ++ "/pubg-utf8.xml").

get_books() ->
    gen_server:call({global, ?MODULE}, {get_books}).
    
get_chapter(BookEncodedName, ChapterNumber) ->
    gen_server:call({global, ?MODULE}, {get_chapter, BookEncodedName, ChapterNumber}).

%% ---------------------------
%% gen_server behaviour
%% ---------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({read_bible, Path}, _From, State) ->
    {reply, ok, maps:put(bible, read_bible_xml(Path), State)};

handle_call({get_books}, _From, State) ->
    #{bible := Bible} = State,
    #{books := Books} = Bible,
    BooksOnly = lists:map(fun(Book) -> #{
                                         full_name => maps:get(full_name, Book),
                                         short_name => maps:get(short_name, Book),
                                         encoded_name => maps:get(encoded_name, Book),
                                         nof_chapters => maps:get(nof_chapters, Book)
                                        }
                          end,
                          Books),
    {reply, BooksOnly, State};

handle_call({get_chapter, BookEncodedName, ChapterNumberBin}, _From, State) ->
    #{bible := Bible} = State,
    FindBookResult = lists:filter(fun(B) -> maps:get(encoded_name, B) == BookEncodedName end, maps:get(books, Bible)),
    case FindBookResult of
        [] -> 
            {reply, {error, book_not_found}, State};
        [Book|_] -> 
            try list_to_integer(binary_to_list(ChapterNumberBin)) of
                ChapterNumber ->
                    Chapters = maps:get(chapters, Book),
                    case ChapterNumber > length(Chapters) orelse ChapterNumber < 1 of
                        true -> {reply, {error, chapter_not_found}, State};
                        false ->
                            Chapter = lists:nth(ChapterNumber, Chapters),
                            {reply, Chapter, State}
                    end 
            catch
                _:_ -> {reply, {error, invalid_chapter_number}, State}
            end
    end.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

init(_Args) ->
    io:format("Starting ~p (~p)~n", [{global, ?MODULE}, self()]),
    {ok, #{}}.

terminate(normal, _State) ->
    ok;

terminate(Reason, _State) ->
    io:format("terminate reason: ~p~n", [Reason]).

%% --------------------
%% Internal
%% -------------------

read_bible_xml(Path) ->
    Xml = erlang:element(1, xmerl_scan:file(Path)),
    Books = get_books_from_xml(Xml),
    
    #{
      xml => Xml,
      books => Books
     }.

get_books_from_xml(BibleXml) ->
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
                book_nt_index => get_bin_attribute("nti", BookNode),
                book_name => get_bin_attribute("n", BookNode),
                encoded_book_name => get_bin_attribute("u", BookNode),
                number => get_bin_attribute("n", ChapterNode),
                verses => Verses
               }
      end, 
      ChapterNodes).    

%% PRIV

get_text(XmlElement) ->
    unicode:characters_to_binary(
      erlang:element(
        3, xmerl_xpath:string("string(text())", XmlElement))).

get_bin_attribute(AttrName, XmlElement) ->
  unicode:characters_to_binary(
    erlang:element(
      3, xmerl_xpath:string("string(@" ++ AttrName ++ ")", XmlElement))).
