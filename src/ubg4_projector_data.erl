-module(ubg4_projector_data).
-behaviour(gen_server).

-export([start_link/0, code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

-export([set_verse/2]).
-export([get_verse/1]).

-define(L(Msg), io:format("~b: ~p~n", [?LINE, Msg])).

%% ---------------------------
%% API
%% ---------------------------

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

set_verse(ProjId, VerseAddressString) ->
    gen_server:cast({global, ?MODULE}, {set_verse, ProjId, VerseAddressString}).

get_verse(ProjId) ->
    gen_server:call({global, ?MODULE}, {get_verse, ProjId}).

%% ---------------------------
%% gen_server behaviour
%% ---------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({get_verse, ProjId}, _From, State) ->
    Val = case maps:find(ProjId, State) of
              {ok, Val0} -> Val0;
              error -> {<<"">>, 0, 0, <<"">>}
          end,
    {reply, Val, State}.

handle_cast({set_verse, ProjId, VerseAddressString}, State) ->
    Verse = ubg4_data:find_verse(VerseAddressString),
    NewState = maps:put(ProjId, Verse, State),
    {noreply, NewState}.

handle_info(_Info, State) ->
    {noreply, State}.

init(_Args) ->
    io:format("Starting ~p (~p)~n", [{global, ?MODULE}, self()]),
    {ok, #{}}.

terminate(normal, _State) ->
    ok;

terminate(Reason, _State) ->
    io:format("terminate reason: ~p~n", [Reason]).
