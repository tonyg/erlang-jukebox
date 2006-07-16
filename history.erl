-module(history).
-behaviour(gen_server).

-export([start/2]).
-export([retrieve/2, record/2, format/3]).

-export([init/1, handle_call/3]).

start(Name, Maxlength) ->
    io:format("Starting history ~p (maxlength ~p).~n", [Name, Maxlength]),
    gen_server:start_link({local, Name}, history, [Maxlength], []).

init([Maxlength]) ->
    {ok, {Maxlength, []}}.

retrieve(Name, N) -> gen_server:call(Name, {retrieve, N}).
record(Name, What) -> gen_server:call(Name, {record, What}).

format(Name, Fmt, Args) ->
    gen_server:call(Name, {record, lists:flatten(io_lib:format(Fmt, Args))}).

handle_call({retrieve, N}, _From, State={_Maxlength, History}) ->
    {reply, lists:sublist(History, 1, N), State};
handle_call({record, What}, _From, State={Maxlength, History}) ->
    {reply, ok, {Maxlength, lists:sublist([What | History], 1, Maxlength)}}.
