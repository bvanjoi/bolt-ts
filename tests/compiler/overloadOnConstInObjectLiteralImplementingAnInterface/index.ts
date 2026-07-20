// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadOnConstInObjectLiteralImplementingAnInterface.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface I {
  x1(a: number, callback: (x: 'hi') => number);
}

var i2: I = { x1: (a: number, cb: (x: 'hi') => number) => { } };

var i3: I = { x1: (a: number, cb: (x: 'hi') => string) => { } };
//~^ ERROR: Type '(a: number, cb: (x: "hi") => string) => void' is not assignable to type '(a: number, callback: (x: "hi") => number) => any'.

var i4: I = { x1: (a: string, cb: (x: 'hi') => number) => { } };
//~^ ERROR: Type '(a: string, cb: (x: "hi") => number) => void' is not assignable to type '(a: number, callback: (x: "hi") => number) => any'.

var i5: I = { x1: (a: number, cb: (x: 'world') => number) => { } };
//~^ ERROR:  Type '(a: number, cb: (x: "world") => number) => void' is not assignable to type '(a: number, callback: (x: "hi") => number) => any'.
