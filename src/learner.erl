-module(learner).
-export([start/3,parse/3]).

% Starts a learner process with a file
% @spec start(list(), string()) -> pid()
start(MainPid, Aggregators, File) -> spawn(?MODULE, parse, [MainPid, Aggregators, File]).

% Parses the file, sends the result to an aggregator
% @spec parse(dict(), {atom(), string()}) -> void
parse(MainPid, Aggregators, {Tag, FileName}) ->
  % Get the tokenized input
  Tokens = input:get_tokenized(FileName),

  % Tally all the tokens
  {TokenCount, Total} = tally(Tokens, dict:new(), 0),

  % Get the aggregator to use
  Aggregator = dict:fetch(Tag, Aggregators),

  % Send everything to the aggregator
  Aggregator ! {results, TokenCount, Total},

  % Tell main we are done
  MainPid ! {done, learner, self()}.


% Tallies the counts of tokens and the total number of
% tokens seen.
% @spec tally(list(), dict(), int()) -> {dict(), int()}
tally([], TokenCount, Total) -> {TokenCount, Total};
tally([Token | Tail], TokenCount, Total) -> tally(Tail, 
  case dict:find(Token, TokenCount) of
    {ok, Count} -> dict:store(Token, Count+1, TokenCount);
    error -> dict:store(Token, 1, TokenCount)
  end, Total+1).

