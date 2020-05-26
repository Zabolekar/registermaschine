"""Yet another Python registermaschine, leveraging the might of Mypy and several other deities.

   arseniiv says he can’t help using extended naturals again.
"""

from __future__ import annotations
from typing import Optional, Tuple, Mapping, Dict, Iterable, Union, Literal, Generator
from collections import defaultdict
from dataclasses import dataclass
from abc import ABC, abstractmethod
from enum import Enum, auto
from functools import lru_cache

class L(Enum): # Enum of various useful singletons
    INF = auto() # Repeat ad nauseam
    HALT = auto() # Halt state instead of halting command
    NOT_DONE = auto() # Bottom

class Nat(int):
    def __init__(self, value: int) -> None:
        if self < 0:
            raise ValueError('A natural number should be nonnegative.')

    def __add__(self, other: int) -> Nat:
        return Nat(int.__add__(self, other))

    def __sub__(self, other: int) -> Nat:
        return Nat(int.__sub__(self, other))

    @staticmethod
    @lru_cache
    def zero() -> Nat:
        return Nat(0)

StepCount = Union[Nat, Literal[L.INF]]

def dec_steps(steps: StepCount) -> Optional[StepCount]:
    if steps is L.INF:
        return steps
    try:
        return Nat(steps - 1)
    except ValueError:
        return None

Reg = int # `Nat` can be out of bounds too, and we could check machines before running anyway
Memory = Dict[Reg, Nat]
State = str
NextState = Union[State, Literal[L.HALT]]

class Cmd(ABC):
    @abstractmethod
    def perform(self, memory: Memory) -> NextState:
        raise NotImplementedError

@dataclass
class Inc(Cmd):
    reg: Reg
    next: NextState

    def perform(self, memory: Memory) -> NextState:
        memory[self.reg] += 1
        return self.next

@dataclass
class Dec(Cmd):
    reg: Reg
    next: NextState

    def perform(self, memory: Memory) -> NextState:
        memory[self.reg] -= 1
        return self.next

@dataclass
class Jeqz(Cmd):
    reg: Reg
    next_if_zero: NextState
    next_if_pos: NextState

    def perform(self, memory: Memory) -> NextState:
        if memory[self.reg]:
            return self.next_if_pos
        return self.next_if_zero

MemorySnapshot = Tuple[Nat, ...]

def memory_snapshot(memory: Memory) -> MemorySnapshot:
    return tuple(memory[k] for k in range(max(memory) + 1))

Machine = Tuple[State, Mapping[State, Cmd]]
PartialResult = Tuple[MemorySnapshot, NextState]
Result = Union[MemorySnapshot, Literal[L.NOT_DONE]]

def run_gen(machine: Machine,
            initial: Iterable[Nat],
            steps: StepCount) -> Generator[PartialResult, None, Result]:
    memory = defaultdict(Nat.zero, enumerate(initial))
    state: NextState
    state, transitions = machine
    while state is not L.HALT:
        if (steps_ := dec_steps(steps)) is None:
            return L.NOT_DONE
        steps = steps_
        state = transitions[state].perform(memory)
        yield memory_snapshot(memory), state
    return memory_snapshot(memory)

def run(machine: Machine, initial: Iterable[Nat], steps: StepCount) -> Result:
    try:
        gen = run_gen(machine, initial, steps)
        while True:
            next(gen)
    except StopIteration as finished:
        return finished.value

adder: Machine = ('a', {
    'a': Jeqz(0, 'd', 'b'),
    'b': Dec(0, 'c'),
    'c': Inc(2, 'a'),
    'd': Jeqz(1, L.HALT, 'e'),
    'e': Dec(1, 'f'),
    'f': Inc(2, 'd')
})

subtractor: Machine = ('a', {
    'a': Jeqz(1, 'e', 'b'),
    'b': Jeqz(0, 'b', 'c'),
    'c': Dec(0, 'd'),
    'd': Dec(1, 'a'),
    'e': Jeqz(0, L.HALT, 'f'),
    'f': Dec(0, 'g'),
    'g': Inc(2, 'e'),
})

def main() -> None:
    print(run(adder, map(Nat, (1, 5, 7)), L.INF)) # (0, 0, 13)
    print(run(subtractor, map(Nat, (8, 3)), L.INF)) # (0, 0, 5)
    print(run(subtractor, map(Nat, (3, 8)), Nat(100))) # L.NOT_DONE

if __name__ == '__main__':
    main()
