// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/dataViewConstructor.ts`, Apache-2.0 License

new DataView(new Uint8Array(32)); // should error
//~^ ERROR: Argument of type 'Uint8Array<ArrayBuffer>' is not assignable to parameter of type 'ArrayBuffer & { BYTES_PER_ELEMENT: never; }'.