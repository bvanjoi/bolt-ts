// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/autoTypeAssignedUsingDestructuringFromNeverNoCrash.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare const b: null;
let file;

if (b === null) {
  // empty
} else {
  [file] = b;
  //~^ ERROR: Type 'never' must have a '[Symbol.iterator]()' method that returns an iterator.
}

file; // request flow type here
