-module(ubg4_data).

-export([read_bible/1, read_bible/0]).
-export([get_books/1]).
-export([test/0]).

read_bible() ->
    read_bible("./priv/pubg-utf8.xml").

read_bible(Path) ->
    erlang:element(1, xmerl_scan:file(Path)).



get_books(Bible) ->
    BNodes = xmerl_xpath:string("/bible/b", Bible),
    lists:map(
      fun(Attr) ->
              #{
                full_name => get_bin_attribute("n", Attr),
                short_name => get_bin_attribute("s", Attr)
               }
      end,
      BNodes).

test() ->
    get_books(read_bible()).
    
%% PRIV

get_bin_attribute(AttrName, XmlAttrElement) ->
  unicode:characters_to_binary(
    erlang:element(
      3, xmerl_xpath:string("string(@" ++ AttrName ++ ")", XmlAttrElement))).
