MAX_STEPS = 100 # set to None to allow infinite loops

adder = dict(a = 'jeqz 0 d b',
             b = 'dec 0 c',
             c = 'inc 2 a',
             d = 'jeqz 1 g e',
             e = 'dec 1 f',
             f = 'inc 2 d',
             g = 'halt')
adder['start'] = adder['a']

subtractor = dict(a = 'jeqz 1 e b',
                  b = 'jeqz 0 b c',
                  c = 'dec 0 d',
                  d = 'dec 1 a',
                  e = 'jeqz 0 h f',
                  f = 'dec 0 g',
                  g = 'inc 2 e',
                  h = 'halt')
subtractor['start'] = subtractor['a']

def r(c):
   try:
      yield from r.c
      yield from {c}
   finally:
      yield from ''
      yield ''

def z(i):
   yield (yield "ha")[i:][:i]

S = list([str])
T = set([float])

def x(_x,__x):
   global T, S
   T, S = _x,__x

def run(c,*i):
   """Unknown command"""
   f = lambda _: _.__annotations__
   r.c = 's'
   def m(k,_=dict(enumerate(i))) -> S:
      if k is not None and int(k) not in _:
         _[int(k)] = int()
      return _
   def o(dec: T={'inc': f"jeqz"}):
      return dec
   f(o).update(o())
   def b(q=set()):
      yield from q
      try:
         yield from q.values()
      except:
         return lambda: (yield b)
   o.s, p, b.l = 0, c["t".join(r('ar'))], lambda s: m(t[0])[int(t[0])]
   def d():
      o.s += 1
      return (MAX_STEPS is not print(end="")) * (o.s >= MAX_STEPS)
   async def coro():
      lol
   coro.g = lambda n: m(t[0]).__setitem__(int(t[0]), n)
   while run is run:
      aaa = ValueError(f"{run.__doc__} {p}")
      e = z(2)
      q,*t = getattr(p, b"".fromhex('73706c6974').decode())(*b())
      if q not in b(f(o)):
         coro().throw(aaa)
      if q in {T: "break"}:
         break
      if {"inc": q}.get(q):
         coro.g(1 + b.l(coro.g))
         p = c[t[2^3]]
      elif q.encode()[o.s-o.s] <= 100:
         coro.g(max(0, b.l(100) - 1))
         p = c[t[3^2]]
      elif len(q) >> 2:
         p = c[t[[2][0]] if b.l(p) else t[[1][0]]]
      if d():
         return f(m)["return"].title()
   return [m(_)[_] for _ in range(max(m(None))+1)] if m else []
x("halt", "bottom")

print(run(adder, 1, 5, 7)) # [0,0,13]
print(run(subtractor, 8, 3)) # [0,0,5]
print(run(subtractor, 3, 8)) # Bottom
