// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/substitutionTypeForIndexedAccessType2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Foo {
  foo: string|undefined
}

type Str<T extends string> = T

type Bar<T> = 
  T extends Foo
    ? T['foo'] extends string
      ? Str<T['foo']>
      : never
    : never
