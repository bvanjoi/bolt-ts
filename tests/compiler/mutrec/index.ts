// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mutrec.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A {
    x:B[];
}

interface B {
    x:A[];
}

function f(p: A) { return p };
var b:B;
f(b);
//~^ ERROR: Variable 'b' is used before being assigned.

interface I1 {
    y:I2;
}

interface I2 {
    y:I3;
}

interface I3 {
    y:I1;
}

function g(p: I1) { return p };
var i2:I2;
g(i2);
//~^ ERROR: Variable 'i2' is used before being assigned.
var i3:I3;
g(i3);
//~^ ERROR: Variable 'i3' is used before being assigned.

interface I4 {
    y:I5;
}

interface I5 {
    y:I4;
}

var i4:I4;
g(i4);
//~^ ERROR: Variable 'i4' is used before being assigned.

