// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mappedTypeUnionConstrainTupleTreatedAsArrayLike.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type HomomorphicMappedType<T> = { [P in keyof T]: T[P] extends string ? boolean : null }

function test1<T extends [number] | [string]>(args: T) {
  const arr: any[] = [] as HomomorphicMappedType<T>
  const arr2: readonly any[] = [] as HomomorphicMappedType<T>
}

function test2<T extends [number] | readonly [string]>(args: T) {
  const arr: any[] = [] as HomomorphicMappedType<T> // error
  //~^ ERROR: Type 'HomomorphicMappedType<T>' is not assignable to type 'any[]'.
  const arr2: readonly any[] = [] as HomomorphicMappedType<T>
}
