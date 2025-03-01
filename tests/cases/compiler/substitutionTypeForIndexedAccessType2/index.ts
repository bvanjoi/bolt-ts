// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/substitutionTypeForIndexedAccessType2.ts`, Apache-2.0 License

interface Foo {
  foo: string|undefined
}

type Str<T extends string> = T

type Bar<T> = 
  T extends Foo
    ? T['foo'] extends string
      // Type 'T["foo"]' does not satisfy the constraint 'string'.
      //  Type 'string | undefined' is not assignable to type 'string'.
      //   Type 'undefined' is not assignable to type 'string'.(2344)
      ? Str<T['foo']>
      : never
    : never

let b: Bar<{foo: string}> = 42
//~^ ERROR: Type 'number' is not assignable to type 'string'.