// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/privatePropertyInUnion.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type Type = string | object;

class SyncableObject {
  private foo: unknown;
}

interface SyncableRef<T extends ISyncableObject> {}

interface ISyncableObject<T = object> extends SyncableObject {}

type __ValueDescriptorType<T extends string | object> = T extends ISyncableObject ? SyncableRef<T> : T;
