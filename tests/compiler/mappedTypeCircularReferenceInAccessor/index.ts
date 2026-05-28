// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mappedTypeCircularReferenceInAccessor.ts`, Apache-2.0 License

//@compiler-options: target=esnext

interface User {
  firstName: string,
  level: number,
  get bestFriend(): User
  set bestFriend(user: SerializablePartial<User>)
}

type FilteredKeys<T> = { [K in keyof T]: T[K] extends number ? K : T[K] extends string ? K : T[K] extends boolean ? K : never }[keyof T];

type SerializablePartial<T> = {
  [K in FilteredKeys<T>]: T[K]
};
