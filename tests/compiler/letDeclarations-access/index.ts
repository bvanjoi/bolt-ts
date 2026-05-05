// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/letDeclarations-access.ts`, Apache-2.0 License

//@compiler-options: target=es6

let x = 0

// No errors

x = 1;
x += 2;
x -= 3;
x *= 4;
x /= 5;
x %= 6;
x <<= 7;
x >>= 8;
x >>>= 9;
x &= 10;
x |= 11;
x ^= 12;

x++;
x--;
++x;
--x;

var a = x + 1;

function f(v: number) { }
f(x);

if (x) { }

x;
(x);

-x;
+x;

x.toString();
