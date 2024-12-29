// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/duplicateInterfaceMembers1.ts`, Apache-2.0 License

interface Bar {
   x: number;
   x: number;
   //~^ ERROR: Duplicate identifier 'x'.
}
