-module(erl_data_set@foreign).

-export([fromList_/1, empty_/0, isEmpty_/1, singleton_/1, out_/1, in_/2, len_/1, join_/2,
         fold_/3, reverse_/1, toList_/1, in_r_/2, out_r_/1]).

fromList_(L) ->
  queue:from_list(L).

empty_() ->
  queue:new().

isEmpty_(Q) ->
  queue:is_empty(Q).

singleton_(E) ->
  fromList_([E]).

out_(Q) ->
  case queue:out(Q) of
    {{value, E1}, Q1} ->
      {just, #{item => E1, queue => Q1}};
    {empty, _Q1} ->
      {nothing}
  end.

in_(E, Q) ->
  queue:in(E, Q).

len_(Q) ->
  queue:len(Q).

join_(Q1, Q2) ->
  queue:join(Q1, Q2).

fold_(F, Acc, Q) ->
  queue:fold(F, Acc, Q).

reverse_(Q) ->
  queue:reverse(Q).

toList_(Q) ->
  queue:to_list(Q).

in_r_(E, Q) ->
  queue:in_r(E, Q).

out_r_(Q) ->
  case queue:out_r(Q) of
    {{value, E1}, Q1} ->
      {just, #{item => E1, queue => Q1}};
    {empty, _Q1} ->
      {nothing}
  end.