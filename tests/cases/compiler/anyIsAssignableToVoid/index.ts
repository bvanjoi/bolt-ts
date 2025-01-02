// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/anyIsAssignableToVoid.ts`, Apache-2.0 License

interface P {
  p: void;
}

interface Q extends P { // check assignability here. any is assignable to void.
  p: any;
}