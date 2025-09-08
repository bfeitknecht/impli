import { File } from "shim";

const encoder = new TextEncoder();
function imp(source) {
  return new File(encoder.encode(source));
}

export default {
  "countdown.imp": imp(`// count down from ten

n := 10;
while n >= 0 do
    print n;
    n := n-1
end`),

  "factorial.imp": imp(`// compute the 10-th factorial (10!)

fac := 1;
while n <= 10 do
    n := n+1;
    fac := fac * n
end;
print fac`),

  "divmod.imp": imp(`// compute the quotient and remainder of d divided by v
procedure divmod(d, v; q, r) begin
    q := 0;
    r := d;
    if not v = 0 then
        while r >= v do
            tmpv := v;
            tmpq := 1;
            while r >= tmpv + tmpv do
                tmpv := tmpv + tmpv;
                tmpq := tmpq + tmpq
            end;
            r := r - tmpv;
            q := q + tmpq
        end
    end
end`),

  "fibonacci.imp": imp(`// linearly compute and print the first 100 fibonacci numbers

f0 := 0;
f1 := 1;
while n < 100 do
    print f0;
    t := f0;
    f0 := f1;
    f1 := f0 + t;
    n := n + 1
end`),

  "gauss.imp": imp(`// compute the sum from 0 to 100

while n < 100 do
    s := s + n;
    n := n + 1
end;
print s`),

  "local.imp": imp(`// local variable definition

x := 42;
var x := x + 17 in
    print x
end;
print x`),

  "nondeterminism.imp": imp(`// random execution

    print 1 [] print 2`),

  "parallel.imp": imp(`// parallel execution

x := 10;
(x := x+1; print x par x := x-1; print x)`),

  "primes.imp": imp(`// compute the n-th prime number
procedure prime(n; p) begin
    // compute the floored square root
    procedure sqrtfloor(n; r) begin
        r := 1;
        temp := 1;
        while temp <= n do
            r *= 2;
            temp := r * r
        end;
        while temp > n do
            r -= 1;
            temp := r * r
        end
    end;
    candidate := 1;
    count := 0;
    while count < n do
        candidate += 1;
        if candidate = 2 or candidate = 3 then
            prime := 1
        else
            i := 2;
            prime := 1;
            sqrtfloor(candidate; sqrt);
            while i <= sqrt and prime = 1 do
                q := candidate / i;
                r := candidate % i;
                if r = 0 then
                    prime := 0
                end;
                if i = 2 then
                    i := 3
                else
                    i += 2
                end
            end
        end;
        if prime = 1 then
            count += 1
        end
    end;
    p := candidate
end`),

  "procedure.imp": imp(`// procedure definition and invocation

procedure fib(n; x) begin
    f0 := 0;
    f1 := 1;
    while k < n do
        t := f0;
        f0 := f1;
        f1 := f0 + t;
        k := k + 1
    end;
    x := f1
end;

fib(10; x);
print x`),

  "turing.imp": imp(`// register machine implemented in IMP to prove its turing completeness

// increment
procedure inc(pc, reg, next; pc, reg) begin
    reg := reg + 1;
    pc := next
end;

// decrement or jump on zero
procedure decjz(pc, reg, dec, label; pc, reg) begin
    if reg = 0 then
        pc := label
    else
        reg := reg - 1;
        pc := dec
    end
end;

// unconditional jump
procedure jump(pc, label; pc) begin
    pc := label
end;

// registers
r0 := 0;
r1 := 3;
pc := 0;
halt := 0;

// copy r1 to r0
while halt = 0 do
    case pc of
        0: decjz(pc, r1, 1, 3; pc, r1),
        1: inc(pc, r0, 0; pc, r0),
        3: halt := 1,
        default: skip
    end
end`),
};
