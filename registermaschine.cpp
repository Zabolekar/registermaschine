#include <vector>
#include <optional>
#include <iostream>
#include <sstream>

// Register value type
using VAL = unsigned int;

// Machine step count limit. Comment out to disable step count checks
#define MAX_STEPS 1000

// Macro definitions

#define MACHINE(name) \
   std::optional<std::vector<VAL>> name(std::vector<VAL>&& memory)

#define GROW_ON_DEMAND(reg) \
   if (reg >= memory.size()) memory.resize(reg+1);

#ifdef MAX_STEPS
   #define START(start) \
      unsigned int steps_left = MAX_STEPS; \
      goto start;
   #define CHECK_STEPS \
      if (!steps_left--) return std::nullopt;
#else
   #define START(start) \
      goto start;
   #define CHECK_STEPS { }
#endif

#define INC(reg, next) \
   CHECK_STEPS \
   GROW_ON_DEMAND(reg) \
   memory[reg]++; \
   goto next;

#define DEC(reg, next) \
   CHECK_STEPS \
   GROW_ON_DEMAND(reg) \
   if (memory[reg] > 0) memory[reg]--; \
   goto next;

#define JEQZ(reg, if_zero, if_nonzero) \
   CHECK_STEPS \
   GROW_ON_DEMAND(reg) \
   if (memory[reg]) goto if_nonzero; \
   else goto if_zero;

#define HALT \
   return memory;

// Machine definitions

MACHINE(adder)
{
  START(a)
  a: JEQZ(0, d, b)
  b: DEC(0, c)
  c: INC(2, a)
  d: JEQZ(1, g, e)
  e: DEC(1, f)
  f: INC(2, d)
  g: HALT
}

MACHINE(subtractor)
{
  START(a)
  a: JEQZ(1, e, b)
  b: JEQZ(0, b, c)
  c: DEC(0, d)
  d: DEC(1, a)
  e: JEQZ(0, h, f)
  f: DEC(0, g)
  g: INC(2, e)
  h: HALT
}

void print(const std::optional<std::vector<VAL>>& maybe_memory)
{
   if (maybe_memory)
   {
      const auto& memory = *maybe_memory;
      const size_t last = memory.size() - 1;
      std::cout << "[";
      for (size_t i = 0; i < last; i++)
         std::cout << memory[i] << ", ";
      std::cout << memory[last] << "]\n";
   }
   else
   {
      std::cout << "Bottom\n";
   }
}

// Usage example

int main()
{
   print(adder({1, 5, 7}));
   print(subtractor({8, 3, 0}));
   print(subtractor({3, 8, 0}));
}
