import options

const maxSteps = 100

type
   OpKind = enum jeqz, inc, dec, halt
   Op = ref object
      case kind: OpKind
      of jeqz:
         conditionRegister: int
         ifZero: Op
         ifNotZero: Op
      of inc, dec:
         valueRegister: int
         next: Op
      of halt:
         nil
   Machine = object
      start: Op

var adder: Machine
block:
   var
      a = Op(kind: jeqz, conditionRegister: 0)
      b = Op(kind: dec, valueRegister: 0)
      c = Op(kind: inc, valueRegister: 2)
      d = Op(kind: jeqz, conditionRegister: 1)
      e = Op(kind: dec, valueRegister: 1)
      f = Op(kind: inc, valueRegister: 2)
      g = Op(kind: halt)
   
   a.ifZero = d
   a.ifNotZero = b
   b.next = c
   c.next = a
   d.ifZero = g
   d.ifNotZero = e
   e.next = f
   f.next = d

   adder.start = a

var subtractor: Machine
block:
   var
      a = Op(kind: jeqz, conditionRegister: 1)
      b = Op(kind: jeqz, conditionRegister: 0)
      c = Op(kind: dec, valueRegister: 0)
      d = Op(kind: dec, valueRegister: 1)
      e = Op(kind: jeqz, conditionRegister: 0)
      f = Op(kind: dec, valueRegister: 0)
      g = Op(kind: inc, valueRegister: 2)
      h = Op(kind: halt)

   a.ifZero = e
   a.ifNotZero = b
   b.ifZero = b
   b.ifNotZero = c
   c.next = d
   d.next = a
   e.ifZero = h
   e.ifNotZero = f
   f.next = g
   g.next = e

   subtractor.start = a

type Results = distinct Option[seq[int]]

proc isSome(r: Results): bool {.borrow.}

proc get(r: Results): seq[int] {.borrow.}

proc `$`(r: Results): string =
   result = if r.isSome: $r.get else: "Bottom"

proc grow(s: var seq[int], index: int) =
   while s.len <= index:
      s &= 0

proc run(machine: Machine, initial_state: seq[int]): Results =

   var
      steps = 0
      op = machine.start
      memory = initial_state
      i = 0

   while steps <= maxSteps:

      if op.kind == inc:
         i = op.valueRegister
         grow(memory, i)
         memory[i] += 1
         op = op.next
      elif op.kind == dec:
         i = op.valueRegister
         grow(memory, i)
         memory[i] -= 1
         if memory[i] < 0:
            memory[i] = 0
         op = op.next
      elif op.kind == jeqz:
         i = op.conditionRegister
         grow(memory, i)
         op = if memory[i] == 0: op.ifZero else: op.ifNotZero
      elif op.kind == halt:
         return Results(some(memory))

      steps += 1

   return Results(none(seq[int]))

echo run(adder, @[1,5,7]) # @[0,0,13]
echo run(subtractor, @[8,3]) # @[0,0,5]
echo run(subtractor, @[3,8]) # Bottom

