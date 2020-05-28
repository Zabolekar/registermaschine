use std::collections::HashMap;

type Reg = usize;
type Val = u64;
type Steps = u64;
type Key = char;

fn main() {
  let adder = Machine {
    start: 'a',
    commands: vec![
      ('a', Command::Jeqz(0, 'd', 'b')),
      ('b', Command::Dec(0, 'c')),
      ('c', Command::Inc(2, 'a')),
      ('d', Command::Jeqz(1, 'g', 'e')),
      ('e', Command::Dec(1, 'f')),
      ('f', Command::Inc(2, 'd')),
      ('g', Command::Halt),
    ].into_iter().collect(),
  };
  
  let subtractor = Machine {
    start: 'a',
    commands: vec![
      ('a', Command::Jeqz(1, 'e', 'b')),
      ('b', Command::Jeqz(0, 'b', 'c')),
      ('c', Command::Dec(0, 'd')),
      ('d', Command::Dec(1, 'a')),
      ('e', Command::Jeqz(0, 'h', 'f')),
      ('f', Command::Dec(0, 'g')),
      ('g', Command::Inc(2, 'e')),
      ('h', Command::Halt),
    ].into_iter().collect(),
  };
  
  println!("{:?}", adder.run(&vec![1, 5, 7], 1000));
  println!("{:?}", subtractor.run(&vec![8, 3], 1000));
  println!("{:?}", subtractor.run(&vec![3, 8], 1000));
}

struct Machine {
  start: Key,
  commands: HashMap<Key, Command>,
}

impl Machine {
  fn run(&self, input: &Vec<Val>, max_steps: Steps) -> Result {
    let mut memory: HashMap<_, _> =
      input.iter().cloned().enumerate().collect();
    let mut command = self.get_command(&self.start);
    for _ in 0..max_steps {
      match command.perform(&mut memory) {
        Some(key) => command = self.get_command(&key),
        None => return Result::Output(self.memory_to_vec(&memory)),
      }
    }
    Result::DoesNotHalt
  }
  
  fn get_command(&self, key: &Key) -> &Command {
    self.commands.get(key)
      .ok_or_else(|| format!("Invalid key: {}", *key))
      .unwrap()
  }
  
  fn memory_to_vec(&self, memory: &HashMap<Reg, Val>) -> Vec<Val> {
    let reg_count = 1 +
      *memory.iter()
        .filter(|(_, &value)| value > 0)
        .map(|(key, _)| key)
        .max()
        .unwrap_or(&0);
    (0..reg_count)
      .map(|reg| memory.get(&reg).unwrap_or(&0))
      .cloned().collect()
  }
}

#[derive(Debug)]
enum Result {
  Output(Vec<Val>),
  DoesNotHalt,
}

enum Command {
  Inc(Reg, Key),
  Dec(Reg, Key),
  Jeqz(Reg, Key, Key),
  Halt,
}

impl Command {
  fn perform(&self, memory: &mut HashMap<Reg, Val>) -> Option<Key> {
    match *self {
      Command::Inc(reg, next) => {
        let register = memory.entry(reg).or_default();
        *register += 1;
        Some(next)
      },
      Command::Dec(reg, next) => {
        let register = memory.entry(reg).or_default();
        if *register > 0 { *register -= 1; }
        Some(next)
      },
      Command::Jeqz(reg, if_zero, if_nonzero) =>
        match memory.get(&reg) {
          None | Some(0) => Some(if_zero),
          _ => Some(if_nonzero)
        },
      Command::Halt => None,
    }
  }
}
