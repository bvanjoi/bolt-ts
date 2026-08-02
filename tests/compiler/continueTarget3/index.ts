// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/continueTarget3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowUnreachableCode

target1:
target2:
while (true) {
  continue target1;
}

const a: string = 42;
//~^ ERROR: Type 'number' is not assignable to type 'string'.