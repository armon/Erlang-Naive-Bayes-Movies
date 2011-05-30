-module(input).
-export([check_input/1, get_files/1, get_tokenized/1]).

% Checks that the input is valid, given a string foldername
% @spec check_input(string()) -> true | false
check_input(FolderName) ->
  IsDirRoot = filelib:is_dir(FolderName),
  SubDirs = [filename:join([FolderName, Sub]) || Sub <- ["neg","pos"]],
  IsDirSubs = [filelib:is_dir(SubDir) || SubDir <- SubDirs],
  AllSubsDirs = lists:foldl(fun(Exist, Accum) -> Exist and Accum end, true, IsDirSubs),
  IsDirRoot and AllSubsDirs.

% Returns all the input file names as a list
% @spec get_files(string()) -> list()
get_files(FolderName) ->
  SubDirs = [{Tag,filename:join([FolderName, Sub])} || {Sub,Tag} <- [{"neg", neg},{"pos", pos}]],
  Files = [[{Tag,File} || File <- filelib:wildcard(filename:join([Dir,"*.txt"]))] || {Tag,Dir} <- SubDirs],
  lists:foldl(fun(More, Accum) -> More ++ Accum end, [], Files).

% Read the file and tokenize it
% @spec get_tokenized(string()) -> list()
get_tokenized(FileName) -> {ok, Data} = file:read_file(FileName), re:split(Data, "[^a-zA-Z0-9]+").

