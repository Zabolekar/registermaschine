using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text.Json;
using static System.Console;

namespace Registermaschine
{
  static class Program
  {
    class Adder : IMachine
    {
      public ICommand Start => a;
      
      static readonly ICommand a = new Jeqz(0, () => d, () => b);
      static readonly ICommand b = new Dec (0, () => c);
      static readonly ICommand c = new Inc (2, () => a);
      static readonly ICommand d = new Jeqz(1, () => g, () => e);
      static readonly ICommand e = new Dec (1, () => f);
      static readonly ICommand f = new Inc (2, () => d);
      static readonly ICommand g = new Halt();
    }
    
    class Subtractor : IMachine
    {
      public ICommand Start => a;
      
      static readonly ICommand a = new Jeqz(1, () => e, () => b);
      static readonly ICommand b = new Jeqz(0, () => b, () => c);
      static readonly ICommand c = new Dec (0, () => d);
      static readonly ICommand d = new Dec (1, () => a);
      static readonly ICommand e = new Jeqz(0, () => h, () => f);
      static readonly ICommand f = new Dec (0, () => g);
      static readonly ICommand g = new Inc (2, () => e);
      static readonly ICommand h = new Halt();
    }
    
    static void Main()
    {
      IMachine adder = new Adder();
      IMachine subtractor = new Subtractor();
      
      WriteLine(adder.Run(new MemoryState(1, 5, 7)));
      WriteLine(subtractor.Run(new MemoryState(8, 3)));
      WriteLine(subtractor.Run(new MemoryState(3, 8)));
    }
  }
  
  
  public interface IMachine
  {
    ICommand Start { get; }
    
    IEnumerable<(MemoryState memory, ICommand? nextCommand)> EnumerateSteps(MemoryState input)
    {
      var (memory, command) = (input, Start);
      while(command is object)
        yield return (memory, command) = command.Perform(memory);
    }
    
    IResult Run(MemoryState input, int? maxSteps = 1000) =>
    {
      var steps = EnumerateSteps(input);
      if(maxSteps is int count)
        steps = steps.Take(count);

      return steps.Last() switch
      {
        (memory: var output, nextCommand: null) => new Success(output),
        _ => new DoesNotHalt()
      };
    }
  }
  
  
  public interface IResult { }
  
  public class Success : IResult
  {
    public Success(MemoryState memoryState) => Output = memoryState;
    public MemoryState Output { get; }
    public override string ToString() => $"Output: {Output}";
  }
  
  public class DoesNotHalt : IResult
  {
    public override string ToString() => "Does not halt";
  }
  
  
  public class MemoryState : IEnumerable<uint>
  {
    public MemoryState(params uint[] values)
      : this(ImmutableDictionary.CreateRange(
        values.Select((v, i) => KeyValuePair.Create((uint) i, v)))) { }
    
    public uint this[uint register] =>
      _registers.GetValueOrDefault(register);
    
    public IEnumerator<uint> GetEnumerator()
    {
      uint lastOccupiedRegister = _registers
        .Where(r => r.Value > 0)
        .Max(r => r.Key);
      for(uint r = 0; r <= lastOccupiedRegister; r++)
        yield return _registers[r];
    }
    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
    
    public override string ToString() =>
      JsonSerializer.Serialize(this);
    
    internal MemoryState SetRegister(uint register, uint value) =>
      new MemoryState(_registers.SetItem(register, value));
    
    MemoryState(IImmutableDictionary<uint, uint> registers) => _registers = registers;
    
    readonly IImmutableDictionary<uint, uint> _registers;
  }
  
  
  public interface ICommand
  {
    (MemoryState memory, ICommand? nextCommand) Perform(MemoryState memory);
  }

  public class Inc : ICommand
  {
    public Inc(uint register, Func<ICommand> nextCommand) =>
      (Register, NextCommand) = (register, nextCommand);
    
    public uint Register { get; }
    public Func<ICommand> NextCommand { get; }
    
    public (MemoryState, ICommand?) Perform(MemoryState memory) =>
    (
      memory.SetRegister(Register, memory[Register] + 1),
      NextCommand()
    );
  }
  
  public class Dec : ICommand
  {
    public Dec(uint register, Func<ICommand> nextCommand) =>
      (Register, NextCommand) = (register, nextCommand);
    
    public uint Register { get; }
    public Func<ICommand> NextCommand { get; }
    
    public (MemoryState, ICommand?) Perform(MemoryState memory) =>
    (
      memory.SetRegister(Register, Math.Max(1, memory[Register]) - 1),
      NextCommand()
    );
  }
  
  public class Jeqz : ICommand
  {
    public Jeqz(uint register, Func<ICommand> ifZero, Func<ICommand> ifNonZero) =>
      (Register, IfZero, IfNonZero) = (register, ifZero, ifNonZero);
    
    public uint Register { get; }
    public Func<ICommand> IfZero { get; }
    public Func<ICommand> IfNonZero { get; }
    
    public (MemoryState, ICommand?) Perform(MemoryState memory) =>
    (
      memory,
      memory[Register] == 0 ? IfZero() : IfNonZero()
    );
  }
  
  public class Halt : ICommand
  {
    public (MemoryState, ICommand?) Perform(MemoryState memory) =>
    (
      memory,
      null
    );
  }
}
