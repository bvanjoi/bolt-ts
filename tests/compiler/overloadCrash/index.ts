// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/overloadCrash.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

interface I1 {a:number; b:number;};
interface I2 {c:number; d:number;};
interface I3 {a:number; b:number; c:number; d:number;};

declare function foo(...n:I1[]);
declare function foo(n1:I2, n3:I2);

var i3:I3;

foo(i3, i3); // should not crash the compiler :)
