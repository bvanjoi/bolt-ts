// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constDeclarations-access3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace M {
    export const x = 0;
}

// Errors
M.x = 1;
//~^ ERROR: Cannot assign to 'x' because it is a read-only property.
M.x += 2;
//~^ ERROR: Cannot assign to 'x' because it is a read-only property.
M.x -= 3;
//~^ ERROR: Cannot assign to 'x' because it is a read-only property.
M.x *= 4;
//~^ ERROR: Cannot assign to 'x' because it is a read-only property.
M.x /= 5;
//~^ ERROR: Cannot assign to 'x' because it is a read-only property.
M.x %= 6;
//~^ ERROR: Cannot assign to 'x' because it is a read-only property.
M.x <<= 7;
//~^ ERROR: Cannot assign to 'x' because it is a read-only property.
M.x >>= 8;
//~^ ERROR: Cannot assign to 'x' because it is a read-only property.
M.x >>>= 9;
//~^ ERROR: Cannot assign to 'x' because it is a read-only property.
M.x &= 10;
//~^ ERROR: Cannot assign to 'x' because it is a read-only property.
M.x |= 11;
//~^ ERROR: Cannot assign to 'x' because it is a read-only property.
M.x ^= 12;
//~^ ERROR: Cannot assign to 'x' because it is a read-only property.

M.x++;
//~^ ERROR: Cannot assign to 'x' because it is a read-only property.
M.x--;
//~^ ERROR: Cannot assign to 'x' because it is a read-only property.
++M.x;
//~^ ERROR: Cannot assign to 'x' because it is a read-only property.
--M.x;
//~^ ERROR: Cannot assign to 'x' because it is a read-only property.

++((M.x));
//~^ ERROR: Cannot assign to 'x' because it is a read-only property.

M["x"] = 0;
//~^ ERROR: Cannot assign to 'x' because it is a read-only property.

// OK
var a = M.x + 1;

function f(v: number) { }
f(M.x);

if (M.x) { }

M.x;
(M.x);

-M.x;
+M.x;

M.x.toString();

