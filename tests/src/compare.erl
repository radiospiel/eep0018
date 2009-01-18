-module(compare).
-export([equiv/2]).

%% Test for equivalence of Erlang terms.
%% Due to arbitrary order of construction, equivalent objects might
%% compare unequal as erlang terms, so we need to carefully recurse
%% through aggregates (tuples and objects).

equiv({error,_}, {error,_}) -> true;
equiv(X, {error,_}) -> false;
equiv({error,_}, X) -> false;

equiv({Props1}, {Props2}) -> equiv_object(Props1, Props2);
equiv({Props1}, X) -> false;
equiv(X, {Props1}) -> false;

equiv(L1, L2) when is_list(L1), is_list(L2) -> equiv_list(L1, L2);
% equiv(N1, N2) when is_number(N1), is_number(N2) -> N1 == N2;
equiv(N1, N2) when is_integer(N1), is_integer(N2) -> N1 == N2;
equiv(N1, N2) when is_float(N1), is_float(N2) -> N1 == N2;
equiv(B1, B2) when is_binary(B1), is_binary(B2) -> B1 == B2;
equiv(true, true) -> true;
equiv(false, false) -> true;
equiv(null, null) -> true;
equiv(_, _) -> false.


%% Object representation and traversal order is unknown.
%% Use the sledgehammer and sort property lists.

equiv_object(Props1, Props2) ->
    L1 = lists:keysort(1, Props1),
    L2 = lists:keysort(1, Props2),
    Pairs = lists:zip(L1, L2),
    true = lists:all(fun({{K1, V1}, {K2, V2}}) ->
                             equiv(K1, K2) and equiv(V1, V2)
                     end, Pairs).

%% Recursively compare tuple elements for equivalence.

equiv_list([], []) ->
    true;
equiv_list([V1 | L1], [V2 | L2]) ->
    case equiv(V1, V2) of
        true ->
            equiv_list(L1, L2);
        false ->
            false
    end.
