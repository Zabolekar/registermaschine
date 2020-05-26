"""Yet another Python registermaschine, leveraging the might of Mypy and several other deities.

   arseniiv says he can’t help using extended naturals again.
"""

from __future__ import annotations
from typing import Optional, Tuple, Mapping, Dict, Iterable, Union, Literal, Iterator
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

def dec_step_count(count: StepCount) -> Optional[StepCount]:
    if count is L.INF:
        return count
    try:
        return Nat(count - 1)
    except ValueError:
        return None

def steps(count: StepCount) -> Iterator[None]:
    while (left := dec_step_count(count)) is not None:
        count = left
        yield None

Reg = int # `Nat` can be out of bounds too, and we could check machines before running anyway
Memory = Dict[Reg, Nat]
UserState = str
State = Union[UserState, Literal[L.HALT]]

class Cmd(ABC):
    @abstractmethod
    def perform(self, memory: Memory) -> State:
        raise NotImplementedError

@dataclass
class Inc(Cmd):
    reg: Reg
    next: State

    def perform(self, memory: Memory) -> State:
        memory[self.reg] += 1
        return self.next

@dataclass
class Dec(Cmd):
    reg: Reg
    next: State

    def perform(self, memory: Memory) -> State:
        try:
            memory[self.reg] -= 1
        except ValueError:
            memory[self.reg] = Nat.zero()
        return self.next

@dataclass
class Jeqz(Cmd):
    reg: Reg
    next_if_zero: State
    next_if_pos: State

    def perform(self, memory: Memory) -> State:
        if memory[self.reg]:
            return self.next_if_pos
        return self.next_if_zero

MemorySnapshot = Tuple[Nat, ...]

def memory_snapshot(memory: Memory) -> MemorySnapshot:
    return tuple(memory[k] for k in range(max(memory) + 1))

Machine = Tuple[State, Mapping[UserState, Cmd]]
PartialResult = Tuple[State, MemorySnapshot]

def run_trace(machine: Machine,
              initial: Iterable[Nat]) -> Iterator[PartialResult]:
    memory = defaultdict(Nat.zero, enumerate(initial))
    state: State
    state, transitions = machine
    while True:
        yield state, memory_snapshot(memory)
        if state is L.HALT:
            break
        state = transitions[state].perform(memory)

Result = Union[MemorySnapshot, Literal[L.NOT_DONE]]

def run(machine: Machine, initial: Iterable[Nat], count: StepCount) -> Result:
    for (state, snapshot), _ in zip(run_trace(machine, initial), steps(count)):
        if state is L.HALT:
            return snapshot
    return L.NOT_DONE

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

if __name__ == '__main__':
    print(run(adder, map(Nat, (1, 5, 7)), L.INF)) # (0, 0, 13)
    print(run(subtractor, map(Nat, (8, 3)), L.INF)) # (0, 0, 5)
    print(run(subtractor, map(Nat, (3, 8)), Nat(100))) # L.NOT_DONE
