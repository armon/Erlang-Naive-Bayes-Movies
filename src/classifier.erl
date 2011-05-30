-module(classifier).
-export([start/2, classify/2]).

% Starts a classifier process. Takes the MainPid to report to,
% and a model object which represents our stored probabilities
% @spec start(pid(), dict()) -> pid()
start(MainPid, Model) -> spawn(?MODULE, classify, [MainPid, Model]).

% Classifies files that we receive, and exits when we get the
% done atom. We take a pid to report to and a model.
% @spec classify(pid(), dict()) -> void
classify(MainPid, Model) ->
  receive
      done -> ok;
      {Tag, File} ->
        % Get the tokenized input
        Tokens = input:get_tokenized(File),

        % Compute the scores
        Scores = [{Type, score(dict:fetch(Type, Model), Tokens)} || Type <- [pos,neg]],

        % Select the max score
        {Predict, _MaxScore} = lists:foldl(
          fun({Type,Score}, {OldPred,OldScore}) ->
            if
              Score > OldScore -> {Type, Score};
              true -> {OldPred, OldScore}
            end
          end,{pos,-1000000},Scores),
        %io:format("~p ~p~n",[Scores, {Predict, _MaxScore}]),

        % Send our prediction to the main thread
        MainPid ! {classify, self(), Tag, Predict, File},
        classify(MainPid, Model)
  end.

% Folds over the tokens to compute an aggregate score
% @spec score(dict(), list()) -> float()
score(Model, Tokens) -> lists:foldl(
    fun(Token, Accum) ->
      Prob = dict:find(Token, Model),
      case Prob of
        error -> Accum;
        {ok, Value} -> Accum + Value
      end
    end, 0, Tokens).

