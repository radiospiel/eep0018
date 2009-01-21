-module(compare).
-export([equiv/2]).
-export([equiv_relaxed/2]).

equiv(X,Y) -> l_do_equiv(strict, X, Y).
equiv_relaxed(X,Y) -> l_do_equiv(relaxed, X, Y).

l_do_equiv(A,B,C) ->
  %io:format("~p ~p ~p ~n", [ A,B,C ]),
  do_equiv(A,B,C).
  
%% Test for equivalence of Erlang terms.
%% Due to arbitrary order of construction, equivalent objects might
%% compare unequal as erlang terms, so we need to carefully recurse
%% through aggregates (tuples and objects).

do_equiv(M, {error,_}, {error,_}) -> true;
do_equiv(M, X, {error,_}) -> false;
do_equiv(M, {error,_}, X) -> false;

do_equiv(M, {}, {}) -> true;

%do_equiv(M, {[{}]}, {[{}]}) -> true;
%do_equiv(M, {[H1|T1]}, {[H2|T2]}) -> equiv_object(M, [H1|T1], [H2|T2]);


do_equiv(M, {Props1}, {Props2}) -> equiv_object(M, Props1, Props2);
do_equiv(M, {Props1}, X) -> false;
do_equiv(M, X, {Props1}) -> false;

do_equiv(M, L1, L2) when is_list(L1), is_list(L2) -> equiv_list(M, L1, L2);

do_equiv(relaxed, N1, N2) when is_number(N1), is_number(N2) -> N1 == N2;
do_equiv(strict, N1, N2) when is_integer(N1), is_integer(N2) -> N1 == N2;
do_equiv(strict, N1, N2) when is_float(N1), is_float(N2) -> N1 == N2;

do_equiv(M, B1, B2) when is_binary(B1), is_binary(B2) -> B1 == B2;
do_equiv(M, B1, B2) when is_atom(B1), is_atom(B2) -> B1 == B2;

do_equiv(M, _, _) -> false.


%% Object representation and traversal order is unknown.
%% Use the sledgehammer and sort property lists.

lists_length([H|T]) -> 1 + lists_length(T);
lists_length([]) -> 0.

equiv_object(M, Props1, Props2) ->
  L1 = lists:keysort(1, Props1),
  L2 = lists:keysort(1, Props2),
  %io:format("~p ~p ~n", [ lists_length(L1), lists_length(L2) ]),
  
  case lists_length(L1) == lists_length(L2) of
    true -> 
      Pairs = lists:zip(L1, L2),
      lists:all(fun(In) ->
        case In of {{K1, V1}, {K2, V2}} ->
          do_equiv(M, K1, K2) and do_equiv(M, V1, V2);
        _ -> io:format("Not matching ~p ~n", [ In ])
        end
      end
      , Pairs);
    false -> false
  end.

%% Recursively compare tuple elements for equivalence.

equiv_list(M, [], []) ->
    true;
equiv_list(M, [V1 | L1], [V2 | L2]) ->
    case l_do_equiv(M, V1, V2) of
        true ->
            equiv_list(M, L1, L2);
        false ->
            false
    end.
