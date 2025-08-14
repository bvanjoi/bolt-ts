// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeOfOperator1.ts`, Apache-2.0 License

var x = 1;
var y: string = typeof x;
var z: number = typeof x;
//~^ ERROR: Type 'string' is not assignable to type 'number' 