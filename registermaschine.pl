checkMaxSteps(N) :- N < 100.
% remove the goal to allow infinite loops
% change the number to change the maximum number of steps

nth(_, [], 0). % if N is out of bounds
nth(0, [H|_], H) :- !.
nth(R, [_|T], M) :- R1 is R-1, nth(R1, T, M).

update(R, N, [], [H|T]) :- update(R, N, [0], [H|T]), !. % if N is out of bounds
update(0, N, [_|T], [N|T]) :- !.
update(R, N, [H|T], [H|T1]) :- R1 is R-1, update(R1, N, T, T1).

eval(Op, MemoryBefore, MemoryAfter) :- eval(Op, MemoryBefore, MemoryAfter, 0).

eval(halt, Memory, Memory, _).
eval(inc(R, Op), MemoryBefore, MemoryAfter, NSteps) :-
   checkMaxSteps(NSteps),
   NStepsNext is NSteps + 1,
   nth(R, MemoryBefore, M),
   N is M+1,
   update(R, N, MemoryBefore, MemoryUpdated),
   eval(Op, MemoryUpdated, MemoryAfter, NStepsNext).
eval(dec(R, Op), MemoryBefore, MemoryAfter, NSteps) :-
   checkMaxSteps(NSteps),
   NStepsNext is NSteps + 1,
   nth(R, MemoryBefore, M),
   (N is M-1, N >= 0, !; N = 0),
   update(R, N, MemoryBefore, Memory),
   eval(Op, Memory, MemoryAfter, NStepsNext).
eval(jeqz(R, IfZero, IfNotZero), MemoryBefore, MemoryAfter, NSteps) :-
   checkMaxSteps(NSteps),
   NStepsNext is NSteps + 1,
   nth(R, MemoryBefore, M),
   (M = 0, !, eval(IfZero, MemoryBefore, MemoryAfter, NStepsNext);
   eval(IfNotZero, MemoryBefore, MemoryAfter, NStepsNext)).

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

:- (adder([1,5,7], X), print(X); print(bottom)), nl. % [0,0,13]
:- (subtractor([8,3], Y), print(Y); print(bottom)), nl. % [0,0,5]
:- (subtractor([3,8], Z), print(Z); print(bottom)), nl. % bottom
