// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/duplicateErrorNameNotFound.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: strict

type RoomInterfae = {};

export type {
    RoomInterface
    //~^ ERROR: Cannot find name 'RoomInterface'.
}