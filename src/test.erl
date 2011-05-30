-module(test).
-export([main/1]).
-define(CLASSIFIER_COUNT, 8).

% Takes an input directory with a folder of examples to test.
% We test all the examples, and emit the classification and our
% accuracy.
main([InputDir]) -> 
  io:format("Testing...~n", []),
  % Load the model
  Model = model_store:load_model(),

  % Get the files, and spawn the learners
  Files = input:get_files(InputDir),

  % Spawn the classifiers
  Classifiers = sets:from_list([classifier:start(self(), Model) || _ <- lists:seq(0, ?CLASSIFIER_COUNT)]),

  % Enqueue the first set
  RemainingFiles = enqueue(Files, sets:to_list(Classifiers)),

  % Wait for the results
  wait_for_classifiers(RemainingFiles, Classifiers, 0, 0),
  io:format("Done");

main(_) ->
  io:format("Invalid arguments given. Only a directory should be provided.").

% Enqueues the first batch of work to be done
% Pumps the classifiers with some work to do, returns the files left to be tested.
% @spec enqueue(list(), list()) -> list()
enqueue([], []) -> [];
enqueue([], [Classifier | MoreClassifiers]) -> Classifier ! done, enqueue([], MoreClassifiers);
enqueue(Files, []) -> Files;
enqueue([File | MoreFiles], [Classifier | MoreClassifiers]) -> Classifier ! File, enqueue(MoreFiles, MoreClassifiers).

% Waits for the classifiers to finish. If we have more files
% We hand those over to be classifier, otherwise we instruct the classifier
% to terminate.
% @spec wait_for_classifiers(list(), set(), int(), int()) -> void
wait_for_classifiers(Files, Classifiers, Correct, Total) ->
  case sets:size(Classifiers) of
      0 -> io:format("Accuracy = ~p~n", [Correct / Total]);
      _ ->
        receive
          {classify, Classifier, Actual, Predict, File} ->
            case Files of
              % Remove the classifier
              [] -> Classifier ! done, More = [], NewClassifiers = sets:del_element(Classifier, Classifiers);

              % Send more work
              [NewFile | More] -> Classifier ! NewFile, NewClassifiers = Classifiers
            end,

            % Print the result
            io:format("~p ~p ~p~n", [File, Predict, Actual]),

            % Recurse
            wait_for_classifiers(More, NewClassifiers, 
              if
                Actual =:= Predict -> Correct + 1;
                true -> Correct
              end
            ,Total+1)
        end
  end.

