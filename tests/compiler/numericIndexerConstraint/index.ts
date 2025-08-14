// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/numericIndexerConstraint.ts`, Apache-2.0 License

class C {
  0: number;
  //~^ ERROR: Property '0' of type 'number' is not assignable to 'number' index type 'RegExp'.
  [x: number]: RegExp;
}