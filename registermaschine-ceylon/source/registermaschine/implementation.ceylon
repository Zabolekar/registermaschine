import ceylon.collection { MutableMap, naturalOrderTreeMap }

shared alias Natural => Integer;
shared alias Register => Integer;
shared alias Memory => MutableMap<Register, Natural>;

shared interface Machine {
    shared formal Op start;
    shared [Natural*]|Bottom run([Natural*] memory, variable CoNat steps = maxSteps) {
        value mapMemory = naturalOrderTreeMap { entries = memory.indexed; };
        variable value op = start;
        while (exists newSteps = steps.prec) {
            if (exists newOp = op.evalStep(mapMemory)) {
            	steps = newSteps;
            	op = newOp;                
                continue;
            }
            value registerCount = (mapMemory.last?.key else -1) + 1;
            return mapMemory.getAll(0:registerCount).defaultNullElements(0).sequence();
        }
        return Bottom.val;
    }
}

shared class Bottom of val {
    shared new val {}
    string => "bottom";
}

shared interface CoNat of Fin | inf {
    shared formal CoNat? prec;
}
shared class Fin(Natural val) satisfies CoNat {
    assert (val >= 0);
    prec => val > 0 then Fin(val - 1);
}
shared object inf satisfies CoNat { prec => this; }

shared interface Op of Inc | Dec | Jeqz | halt {
    shared formal Op? evalStep(Memory m);
}
shared class Inc(Register r, Op() next) satisfies Op {
    shared actual Op evalStep(Memory m) {
        m[r] = (m[r] else 0) + 1;
        return next();
    }
}
shared class Dec(Register r, Op() next) satisfies Op {
    shared actual Op evalStep(Memory m) {
        m[r] = largest((m[r] else 0) - 1, 0);
        return next();
    }
}
shared class Jeqz(Register r, Op() ifZero, Op() ifNonzero) satisfies Op {
    shared actual Op evalStep(Memory m) =>
        if ((m[r] else 0) == 0) then ifZero() else ifNonzero();
}
shared object halt satisfies Op {
    shared actual Null evalStep(Memory m) => null;
}
