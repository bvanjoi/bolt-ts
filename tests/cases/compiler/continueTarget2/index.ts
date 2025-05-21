// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/continueTarget2.ts`, Apache-2.0 License

target:
while (true) {
  continue target;
}

let a: string = 42;
//~^ ERROR: Type 'number' is not assignable to type 'string'.