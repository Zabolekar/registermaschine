import options
import macros

const maxSteps = 100

template defOp(opName, kind: untyped): untyped =
   varSec.add(newIdentDefs(opName,
                           newEmptyNode(),
                           newNimNode(nnkObjConstr)
                           .add(ident("Op"))
                           .add(newColonExpr(ident("kind"), kind))))

template assignment(a: string, b: untyped): untyped =
   myBlock.add(newAssignment(newDotExpr(opname, ident(a)), b))

macro machine(name: untyped, body: untyped): untyped =

   var myBlock: NimNode = newNimNode(nnkStmtList)
   var varSec: NimNode = newNimNode(nnkVarSection)
   myBlock.add(varSec)

   for line in body:
      if $line[0] == "start":
         myBlock.add(newAssignment(newDotExpr(name, ident("start")), line[1]))
      else:
         var
            opname = line[0]
            op = line[1]
         if op == ident("halt"):
            defOp(opName, op)
         else:
            var
               kind = op[0]
               rest = op[1]
            defOp(opName, kind)
            case $kind:
               of "inc", "dec":
                  varSec[^1][2].add(newColonExpr(ident("valueRegister"), rest[0]))
                  assignment("next", rest[1])
               of "jeqz":
                  varSec[^1][2].add(newColonExpr(ident("conditionRegister"), rest[0]))
                  assignment("ifZero", rest[1][0]) 
                  assignment("ifNotZero", rest[1][1])
               else:
                  error("unknown kind: " & $kind)

   result = quote do:
      var `name`: Machine
      block:
         `myBlock`

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

      case op.kind:
         of inc:
            i = op.valueRegister
            grow(memory, i)
            memory[i] += 1
            op = op.next
         of dec:
            i = op.valueRegister
            grow(memory, i)
            memory[i] -= 1
            if memory[i] < 0:
               memory[i] = 0
            op = op.next
         of jeqz:
            i = op.conditionRegister
            grow(memory, i)
            op = if memory[i] == 0: op.ifZero else: op.ifNotZero
         of halt:
            return Results(some(memory))

      steps += 1

   return Results(none(seq[int]))

machine adder:
   a = jeqz 0 d b
   b = dec 0 c
   c = inc 2 a
   d = jeqz 1 g e
   e = dec 1 f
   f = inc 2 d
   g = halt
   start = a

machine subtractor:
   a = jeqz 1 e b
   b = jeqz 0 b c
   c = dec 0 d
   d = dec 1 a
   e = jeqz 0 h f
   f = dec 0 g
   g = inc 2 e
   h = halt
   start = a

echo run(adder, @[1,5,7]) # @[0,0,13]
echo run(subtractor, @[8,3]) # @[0,0,5]
echo run(subtractor, @[3,8]) # Bottom

