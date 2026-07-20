// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/restParamUsingMappedTypeOverUnionConstraint.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit


type HomomorphicMappedType<T> = { [P in keyof T]: T[P] extends string ? boolean : null }

declare function test<T extends [number] | [string]>(
  args: T,
  fn: (...args: HomomorphicMappedType<T>) => void
): void
