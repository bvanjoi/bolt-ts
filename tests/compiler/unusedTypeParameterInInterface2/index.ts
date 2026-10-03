// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedTypeParameterInInterface2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

interface int<T, U, V> {
  //~^ ERROR: 'U' is declared but its value is never read.
    f1(a: T): string;
    c: V;
}
