// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/promiseEmptyTupleNoException.ts`, Apache-2.0 License

//@compiler-options: target=es2017
//@compiler-options: strict=false

export async function get(): Promise<[]> {
  let emails = [];
  return emails;
  //~^ ERROR: Type 'any[]' is not assignable to type '[]'.
}
