// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericCallOnMemberReturningClosedOverObject.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/11584

function example<T1>() {
  let x = {
    foo: <T2>(t2: T2) => x,
    bar: (t1: T1) => x,
  };
  return x;
}

example<number>().foo("hello").bar(1);
