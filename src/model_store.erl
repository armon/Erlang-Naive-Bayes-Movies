-module(model_store).
-export([start/1,get_results/1,load_model/0]).

% Starts the Model Store process. Waits for the
% Positive and negative aggregate to arrive, then creates
% the model file.
% @spec start(pid()) -> pid()
start(MainPid) -> spawn(?MODULE, get_results, [MainPid]).

% Gets the results and writes them to the model file.
% Sends the done atom to the main pid when it is finished.
get_results(MainPid) ->
  Summary = wait_for_summary(dict:new()),
  file:write_file("model",term_to_binary(Summary)),
  MainPid ! {done, model_store, self()}.

% Waits to receive the summary of both
% pos and negatie
wait_for_summary(Dict) ->
  case dict:size(Dict) of
    2 -> Dict;
    _ ->
      receive
        {Term, Sum} ->
          NewDict = dict:store(Term,Sum,Dict),
          io:format("Received aggregate for ~p.~n",[Term]),
          wait_for_summary(NewDict)
      end
  end.

% Loads the model.
% @spec load_model() -> term()
load_model() ->
  {ok, Raw} = file:read_file("model"),
  binary_to_term(Raw).


