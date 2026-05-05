// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noObjectKeysToKeyofT.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noStrictGenericChecks

type A = <T, U>(x: T, y: U) => [T, U];
type B = <S>(x: S, y: S) => [S, S];

function f(a: A, b: B) {
    a = b;  // Error disabled here
    //~^ ERROR: Type 'B' is not assignable to type 'A'.
    b = a;  // Ok
}
