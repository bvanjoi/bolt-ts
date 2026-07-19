// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferentialTypingObjectLiteralMethod2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface Int<T, U> {
  [s: string]: (x: T) => U;
}
declare function foo<T, U>(x: T, y: Int<T, U>, z: Int<U, T>): T;
foo("", { method(p1) { return p1.length } }, { method(p2) { return undefined } });
foo("", { method(p1) { return p1.length } }, { method(p2) { 
  let s2: string = p2;
  //~^ ERROR: Type 'number' is not assignable to type 'string'
  return undefined 
}});
