#include <stdio.h>

/* Machine step count limit.
   Comment out to disable step count checks */
#define MAX_STEPS 1000

/* Register value type */
#define VAL unsigned int


/* Macro definitions */

#define MACHINE(name) \
  int name(VAL memory[])

#ifdef MAX_STEPS

  #define START(start) \
    unsigned int steps_left = MAX_STEPS; \
    goto start;
  
  #define CHECK_STEPS \
    if(!steps_left--) return 0;

#else

  #define START(start) \
    goto start;
  
  #define CHECK_STEPS { }

#endif

#define INC(reg, next) \
  CHECK_STEPS \
  memory[reg]++; \
  goto next;

#define DEC(reg, next) \
  CHECK_STEPS \
  if(memory[reg] > 0) memory[reg]--; \
  goto next;

#define JEQZ(reg, if_zero, if_nonzero) \
  CHECK_STEPS \
  if(memory[reg]) goto if_nonzero; \
  else goto if_zero;

#define HALT \
  return 1;


/* Machine definitions */

MACHINE(adder) {
  START(a)
  a: JEQZ(0, d, b)
  b: DEC(0, c)
  c: INC(2, a)
  d: JEQZ(1, g, e)
  e: DEC(1, f)
  f: INC(2, d)
  g: HALT
}

MACHINE(subtractor) {
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


/* Usage example */

int main(void) {
  
  VAL mem1[] = { 1, 5, 7 };
  if(adder(mem1))
    printf("[%d, %d, %d]\n", mem1[0], mem1[1], mem1[2]);
  else
    printf("bottom\n");
  
  VAL mem2[] = { 8, 3, 0 };
  if(subtractor(mem2))
    printf("[%d, %d, %d]\n", mem2[0], mem2[1], mem2[2]);
  else
    printf("bottom\n");

  VAL mem3[] = { 3, 8, 0 };
  if(subtractor(mem3))
    printf("[%d, %d, %d]\n", mem3[0], mem3[1], mem3[2]);
  else
    printf("bottom\n");
  
  return 0;
}
