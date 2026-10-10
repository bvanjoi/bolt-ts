// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/genericIndexTypeHasSensibleErrorMessage.ts`, Apache-2.0 License

type Wat<T extends string> = { [x: T]: string };
//~^ ERROR: An index signature parameter type cannot be a literal type or generic type. Consider using a mapped object type instead.

type Client<K extends string> = { db: K } & { [P in K]: (data: P) => void }

function f<T extends 'a' | 'b'>(c: Client<T>, name: T) {
  c[name]('a')
  //~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'never'.
}