-module(factorial).

-export([ factorial/1
        , factorial/2
        , factorial_naive/1
        , factorial_memoized/1
        ]).

-define(DEFAULT_IMPL, fun factorial_naive/1).

-spec factorial_naive(non_neg_integer()) -> non_neg_integer().
factorial_naive(N) when N > 0, is_integer(N) -> N * factorial_naive(N-1);
factorial_naive(0) -> 1.

-spec factorial_memoized(non_neg_integer()) -> non_neg_integer().
factorial_memoized(0)  -> 1;
factorial_memoized(1)  -> 1;
factorial_memoized(2)  -> 2;
factorial_memoized(3)  -> 6;
factorial_memoized(4)  -> 24;
factorial_memoized(5)  -> 120;
factorial_memoized(6)  -> 720;
factorial_memoized(7)  -> 5040;
factorial_memoized(8)  -> 40320;
factorial_memoized(9)  -> 362880;
factorial_memoized(10) -> 3628800;
factorial_memoized(11) -> 39916800;
factorial_memoized(12) -> 479001600;
factorial_memoized(13) -> 6227020800;
factorial_memoized(14) -> 87178291200;
factorial_memoized(15) -> 1307674368000;
factorial_memoized(16) -> 20922789888000;
factorial_memoized(17) -> 355687428096000;
factorial_memoized(18) -> 6402373705728000;
factorial_memoized(19) -> 121645100408832000;
factorial_memoized(20) -> 2432902008176640000;
%% Maybe just maybe, this is a bug... o_O
factorial_memoized(N) when N > 20, is_integer(N) -> factorial_naive(N).

factorial(N) when N > 0, is_integer(N) -> ?DEFAULT_IMPL(N);
factorial(0) -> 1.

factorial(N, Algo) when N > 0, is_integer(N) -> Algo(N);
factorial(0, _) -> 1.
