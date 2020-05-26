from collections import defaultdict

MAX_STEPS = 100 # set to None to allow infinite loops

adder = dict(a = ('jeqz', 0, 'd', 'b'),
             b = ('dec', 0, 'c'),
             c = ('inc', 2, 'a'),
             d = ('jeqz', 1, 'g', 'e'),
             e = ('dec', 1, 'f'),
             f = ('inc', 2, 'd'),
             g = 'halt')
adder['start'] = adder['a']

subtractor = dict(a = ('jeqz', 1, 'e', 'b'),
                  b = ('jeqz', 0, 'b', 'c'),
                  c = ('dec', 0, 'd'),
                  d = ('dec', 1, 'a'),
                  e = ('jeqz', 0, 'h', 'f'),
                  f = ('dec', 0, 'g'),
                  g = ('inc', 2, 'e'),
                  h = 'halt')
subtractor['start'] = subtractor['a']

def run(machine, initial_state):

   memory = defaultdict(int, enumerate(initial_state))

   steps = 0

   op = machine['start']

   while op != 'halt':
      
      if op[0] == 'inc':
         memory[op[1]] += 1
         op = machine[op[2]]
      elif op[0] == 'dec':
         memory[op[1]] = max(0, memory[op[1]]-1)
         op = machine[op[2]]
      elif op[0] == 'jeqz':
         op = machine[op[2 + bool(memory[op[1]])]]
      else:
         raise ValueError(f"Unknown command {op}")

      steps += 1

      if MAX_STEPS is not None and steps >= MAX_STEPS:
         # The flow diagram did not halt for given input after max_steps steps
         return "Bottom"

   if memory:
      return [memory[k] for k in range(max(memory)+1)]
   else:
      return [] # to handle special cases like run(dict(start='halt'), [])

if __name__ == "__main__":
   print(run(adder, [1,5,7])) # [0,0,13]
   print(run(subtractor, [8,3])) # [0,0,5]
   print(run(subtractor, [3,8])) # Bottom
