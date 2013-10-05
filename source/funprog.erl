-module(funprog).

-export([compose/2]).

-spec compose(G, F) -> fun((A) -> C)
when G :: fun((B) -> C),
     F :: fun((A) -> B),
     A :: any(),
     B :: any(),
     C :: any().
compose(G, F) ->
  fun(X) -> G(F(X)) end.
