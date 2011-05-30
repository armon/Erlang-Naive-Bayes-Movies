-module(aggregate).
-export([start/2,aggregate/2]).

% Starts a new aggregator, given the model pid to send
% results to and the type to aggregate (pos, neg).
% @spec start(pid(), atom()) -> pid()
start(ModelPid, Type) -> spawn(?MODULE, aggregate, [ModelPid, Type]).

% Waits to receive all the sub-counts, and then reduces them to
% a log-based propotion of the corpus. Sends the results to the
% model writer.
aggregate(ModelPid, Type) ->
  % Get the word counts, and totals
  {Counts, Total} = wait_for_results(dict:new(), 0),

  % Convert to an log base of the proportion
  Frequency = dict:map(fun(_Key, Val) -> 1.0/(-1*math:log(Val / Total)) end, Counts),

  % Send this to the model writer
  ModelPid ! {Type, Frequency}.

% Accumulates the counts from all the children that send it to us.
wait_for_results(Dict, Total) ->
  receive
    flush -> {Dict, Total};
    {results, Summary, More} ->
      % Merge the dictionaries, add to the total
      NewDict = dict:merge(fun(_Key, Val1, Val2) -> Val1+Val2 end, Dict, Summary),
      wait_for_results(NewDict, Total+More)
  end.



