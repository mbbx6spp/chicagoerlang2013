%% There is a problem in these properties. Find it, fix it and then rerun! :)
-module(algebraic_properties).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

%% HINT: See below
%non_empty_list(G) ->
%  [G | list(G)].

prop_list_sort_idempotent() ->
  ?FORALL(L, list(int()), lists:sort(L) == lists:sort(lists:sort(L))).

prop_list_sort_length() ->
  ?FORALL(L, list(int()), length(lists:sort(L)) == length(L)).

prop_list_sort_last_is_max() ->
  ?FORALL(L, list(int()), lists:last(lists:sort(L)) == lists:max(L)).

prop_list_sort_head_is_min() ->
  ?FORALL(L, list(int()), hd(lists:sort(L)) == lists:min(L)).

main() ->
  true = eqc:quickcheck(algebraic_properties:prop_list_sort_idempotent()),
  true = eqc:quickcheck(algebraic_properties:prop_list_sort_length()),
  true = eqc:quickcheck(algebraic_properties:prop_list_sort_last_is_max()),
  true = eqc:quickcheck(algebraic_properties:prop_list_sort_head_is_min()),
  ok.
