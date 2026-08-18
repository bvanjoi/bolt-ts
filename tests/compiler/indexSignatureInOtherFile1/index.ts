// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexSignatureInOtherFile1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Test extends Array1 {
  [key: symbol]: string
  //~^ ERROR: Property '[...]' of type '() => IterableIterator<any, any, any>' is not assignable to 'symbol' index type 'string'.
  //~| ERROR: Property '[...]' of type '() => { copyWithin: boolean; entries: boolean; fill: boolean; find: boolean; findIndex: boolean; keys: boolean; values: boolean; }' is not assignable to 'symbol' index type 'string'.
}