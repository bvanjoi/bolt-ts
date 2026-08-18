// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/multiLineErrors.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var t = 32;

function noReturn(): {
  //~^ ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.
    n: string;
    y: number;
}
{
    var x = 4;
    var y = 10;
}

interface A1 {
    x: { y: number; };
}
interface A2 {
    x: { y: string; };
}

declare var t1: A1;
declare var t2: A2;
t1 = t2;
//~^ ERROR: Type 'A2' is not assignable to type 'A1'.