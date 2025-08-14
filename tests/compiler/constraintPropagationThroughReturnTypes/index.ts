// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/constraintPropagationThroughReturnTypes.ts`, Apache-2.0 License

function g<T>(x: T): T {
  return x;
}
 
function f<S extends { foo: string }>(x: S) {
  var y = g(x);
  y;
}

{
  const a: string = ''
  type S<T> = T extends {[a]: never} ? never : T;
  const test = <T>(_: S<T>): void => {};
  test({key: 'value'});
}

{
  const a: string = ''
  type S<T> = T extends {[a]?: never} ? never : T;
  const test = <T>(_: S<T>): void => {};
  test({key: 'value'});
}