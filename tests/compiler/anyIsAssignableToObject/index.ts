// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/anyIsAssignableToObject.ts`, Apache-2.0 License

interface P {
  p: {};
}

interface Q extends P { // Check assignability here. Any is assignable to {}
  p: any;
}