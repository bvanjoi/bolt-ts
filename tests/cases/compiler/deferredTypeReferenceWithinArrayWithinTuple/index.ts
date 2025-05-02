// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/deferredTypeReferenceWithinArrayWithinTuple.ts`, Apache-2.0 License

type TypeStructure =
  | ["or", TypeStructure[]] // problem is only here, when using array
  | ["string"]
  | ["number"] 
  | ["list", TypeStructure] // with just this it is ok

type A = true extends true ? | number | string : false;
  