'use strict';

const maxSteps = 100;

const adder = {
  start: 'a',
  commands: {
    a: jeqz(0, 'd', 'b'),
    b: dec(0, 'c'),
    c: inc(2, 'a'),
    d: jeqz(1, 'g', 'e'),
    e: dec(1, 'f'),
    f: inc(2, 'd'),
    g: halt()
  }
};

const subtractor = {
  start: 'a',
  commands: {
    a: jeqz(1, 'e', 'b'),
    b: jeqz(0, 'b', 'c'),
    c: dec(0, 'd'),
    d: dec(1, 'a'),
    e: jeqz(0, 'h', 'f'),
    f: dec(0, 'g'),
    g: inc(2, 'e'),
    h: halt()
  }
};

console.log(run(adder, [1, 5, 7]));
console.log(run(subtractor, [8, 3]));
console.log(run(subtractor, [3, 8]));

function run(machine, input) {
  let memory = input;
  let command = machine.commands[machine.start];
  let stepsLeft = maxSteps;
  while(command){
    command = command(machine.commands, memory);
    if(!stepsLeft--)
      return "bottom";
  }
  return memory;
}

function inc(reg, next) {
  return (commands, memory) => {
    memory[reg] = (memory[reg] || 0) + 1;
    return commands[next];
  };
}

function dec(reg, next) {
  return (commands, memory) => {
    memory[reg] = Math.max(0, (memory[reg] || 0) - 1);
    return commands[next];
  };
}

function jeqz(reg, ifZero, ifNonZero) {
  return (commands, memory) =>
    commands[memory[reg] == 0 ? ifZero : ifNonZero];
}

function halt() {
  return (commands, memory) =>
    null;
}
