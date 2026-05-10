// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/i3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I3 { one?: number; }; 
declare var x: {one: number};
declare var i: I3;

i = x;
x = i;
///~^ ERROR: ype 'I3' is not assignable to type '{ one: number; }'.