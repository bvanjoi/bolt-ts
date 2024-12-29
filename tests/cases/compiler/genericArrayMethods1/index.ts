// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/genericArrayMethods1.ts`, Apache-2.0 License

var x:string[] =  [0,1].slice(0);
//~^ ERROR: Type 'number[]' is not assignable to type 'string[]'.
