// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/switchCases.ts`, Apache-2.0 License

switch(0) {
 case 1:
  //~^ ERROR: Type '1' is not comparable to type '0'.
 break;
}
