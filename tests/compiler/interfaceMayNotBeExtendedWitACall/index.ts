// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/interfaceMayNotBeExtendedWitACall.ts`, Apache-2.0 License

interface color {}

interface blue extends color() { // error
  //~^ ERROR: Expected ','.
  //~| ERROR: Identifier expected.
  //~| ERROR: Expected ','.

}
