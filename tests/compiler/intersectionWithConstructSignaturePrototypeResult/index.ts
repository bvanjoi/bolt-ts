// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/intersectionWithConflictingPrivates.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare class EmberObject {}

type PersonType = Readonly<typeof EmberObject> &
  (new (properties?: object) => {
    firstName: string;
    lastName: string;
  } & EmberObject) &
  (new (...args: any[]) => {
    firstName: string;
    lastName: string;
  } & EmberObject);

type PersonPrototype = PersonType["prototype"];
