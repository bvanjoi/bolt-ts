// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/continueInIterationStatement3.ts`, Apache-2.0 License

for (;;) {
  continue;
}

let a: string = 42;
//~^ ERROR: Type 'number' is not assignable to type 'string'.