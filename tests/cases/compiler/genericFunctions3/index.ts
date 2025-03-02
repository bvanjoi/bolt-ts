// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericFunctions3.ts`, Apache-2.0 License

//@ run-fail

interface Query<T> {
  foo(x: string): Query<T[]>;
}

function from<T>(arg: boolean): Query<T>; // was Error: Overload signature is not compatible with function definition.
function from<T>(arg: any): Query<T> {
  return undefined;
}

let ff: Query<string[]> = from<string>(false).foo("")