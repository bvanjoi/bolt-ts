// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constDeclarations-access2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

const x = 0

// Errors
x = 1;
//~^ ERROR: Cannot assign to 'x' because it is a constant.
x += 2;
//~^ ERROR: Cannot assign to 'x' because it is a constant.
x -= 3;
//~^ ERROR: Cannot assign to 'x' because it is a constant.
x *= 4;
//~^ ERROR: Cannot assign to 'x' because it is a constant.
x /= 5;
//~^ ERROR: Cannot assign to 'x' because it is a constant.
x %= 6;
//~^ ERROR: Cannot assign to 'x' because it is a constant.
x <<= 7;
//~^ ERROR: Cannot assign to 'x' because it is a constant.
x >>= 8;
//~^ ERROR: Cannot assign to 'x' because it is a constant.
x >>>= 9;
//~^ ERROR: Cannot assign to 'x' because it is a constant.
x &= 10;
//~^ ERROR: Cannot assign to 'x' because it is a constant.
x |= 11;
//~^ ERROR: Cannot assign to 'x' because it is a constant.
x ^= 12;
//~^ ERROR: Cannot assign to 'x' because it is a constant.

x++;
//~^ ERROR: Cannot assign to 'x' because it is a constant.
x--;
//~^ ERROR: Cannot assign to 'x' because it is a constant.
++x;
//~^ ERROR: Cannot assign to 'x' because it is a constant.
--x;
//~^ ERROR: Cannot assign to 'x' because it is a constant.

++((x));
//~^ ERROR: Cannot assign to 'x' because it is a constant.

// OK
var a = x + 1;

function f(v: number) { }
f(x);

if (x) { }

x;
(x);

-x;
+x;

x.toString();
