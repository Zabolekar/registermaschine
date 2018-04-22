CoNat maxSteps = Fin(100); // `inf` if you want to allow infinite loops

shared void run() {
    print(adder.run([1, 5, 7])); // [0, 0, 13]
    print(subtractor.run([8, 3])); // [0, 0, 5]
    print(subtractor.run([3, 8])); // bottom
}

object adder satisfies Machine {
    start => a();
    Op a() => Jeqz(0, d, b);
    Op b() => Dec(0, c);
    Op c() => Inc(2, a);
    Op d() => Jeqz(1, g, e);
    Op e() => Dec(1, f);
    Op f() => Inc(2, d);
    Op g() => halt; 
}

object subtractor satisfies Machine {
    start => a();
    Op a() => Jeqz(1, e, b);
    Op b() => Jeqz(0, b, c);
    Op c() => Dec(0, d);
    Op d() => Dec(1, a);
    Op e() => Jeqz(0, h, f);
    Op f() => Dec(0, g);
    Op g() => Inc(2, e);
    Op h() => halt;
}
