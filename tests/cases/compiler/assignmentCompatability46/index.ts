// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/assignmentCompatability46.ts`, Apache-2.0 License

declare function fn(x: never): void;

fn([1, 2, 3])
//~^ ERROR: Argument of type 'number[]' is not assignable to parameter of type 'never'.
fn({ a: 1, b: 2 })
//~^ ERROR: Argument of type '{ b: number; a: number; }' is not assignable to parameter of type 'never'.
