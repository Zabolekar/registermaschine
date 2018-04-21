nth(_, [], 0). % if N is out of bounds
nth(0, [H|_], H) :- !.
nth(N, [_|T], M) :- N1 is N-1, nth(N1, T, M).

update(N, X, [], [H|T]) :- update(N, X, [0], [H|T]), !. % if N is out of bounds
update(0, X, [_|T], [X|T]) :- !.
update(N, X, [H|T], [H|T1]) :- N1 is N-1, update(N1, X, T, T1).

eval(halt, Memory, Memory).
eval(inc(R, Op), MemoryBefore, MemoryAfter) :-
   nth(R, MemoryBefore, M),
   N is M+1,
   update(R, N, MemoryBefore, MemoryUpdated),
   eval(Op, MemoryUpdated, MemoryAfter).
eval(dec(R, Op), MemoryBefore, MemoryAfter) :-
   nth(R, MemoryBefore, M),
   (N is M-1, N >= 0, !; N is 0),
   update(R, N, MemoryBefore, Memory),
   eval(Op, Memory, MemoryAfter).
eval(jeqz(R, IfZero, IfNotZero), MemoryBefore, MemoryAfter) :-
   nth(R, MemoryBefore, M),
   (M is 0, eval(IfZero, MemoryBefore, MemoryAfter), !;
   eval(IfNotZero, MemoryBefore, MemoryAfter)).

adder(MemoryBefore, MemoryAfter) :-
   A = jeqz(0, D, B),
   B = dec(0, C),
   C = inc(2, A),
   D = jeqz(1, G, E),
   E = dec(1, F),
   F = inc(2, D),
   G = halt,
   eval(A, MemoryBefore, MemoryAfter).
    
subtractor(MemoryBefore, MemoryAfter) :-
   A = jeqz(1, E, B),
   B = jeqz(0, B, C),
   C = dec(0, D),
   D = dec(1, A),
   E = jeqz(0, H, F),
   F = dec(0, G),
   G = inc(2, E),
   H = halt,
   eval(A, MemoryBefore, MemoryAfter).

:- adder([1,5,7], X), print(X), nl,
   subtractor([8,3], Y), print(Y), nl,
   !.
