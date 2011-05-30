-module(train).
-export([main/1]).

% Takes an input directory with training examples
% We learn from our examples and produce a model file
% That can be used to test future examples.
main([InputDir]) -> 
  io:format("Training...~n", []),
  
  % Start the model writer
  ModelWriterPid = model_store:start(self()),

  % Start the aggregators
  Aggregators = dict:from_list([{Type, aggregate:start(ModelWriterPid, Type)} || Type <- [pos, neg]]),

  % Get the files, and spawn the learners
  Files = input:get_files(InputDir),
  Learners = sets:from_list([learner:start(self(), Aggregators, File) || File <- Files]),

  % Wait for learners to finish
  wait_for_learners(Learners),

  % Flush the aggregated data
  [Agg ! flush || {_Type,Agg} <- dict:to_list(Aggregators)],

  % Wait for termination
  receive
    {done, model_store, ModelWriterPid} -> done
  end;

main(_Args) ->
  io:format("Invalid arguments given. Only a directory should be provided.").

% Waits for all of our learners to terminate with their file.
% @spec wait_for_learners(set()) -> done
wait_for_learners(Learners) ->
  case sets:size(Learners) of
    0 -> done;
    _ ->
      receive
        {done, learner, LearnPid} ->
          NewLearners = sets:del_element(LearnPid, Learners),
          wait_for_learners(NewLearners)
      end
  end.

