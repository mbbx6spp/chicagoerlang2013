-module(either).

-export([ either/3
        , either/2
        , lefts/1
        , rights/1
        , partitionEithers/1
        ]).


either(FunLeft, FunRight, Val) ->
  case Val of
    {ok, X} -> FunRight(X);
    {error, Err} -> FunLeft(Err)
  end.

either(FunLeft, FunRight) ->
  fun({error, {Type, Reason}})-> FunLeft([atom_to_list(Type), ": ", Reason]);
     ({error, Error})         -> FunLeft(Error);
     ({ok, State})            -> FunRight(State);
     ({ok, State, _})         -> FunRight(State)
  end.

lefts(Eithers) ->
  [ Err || {error, Err} <- Eithers ].

rights(Eithers) ->
  [ X || {ok, X} <- Eithers ].

partitionEithers(Eithers) ->
  { lefts(Eithers), rights(Eithers) }.
