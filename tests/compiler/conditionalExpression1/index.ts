// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/conditionalExpression1.ts`, Apache-2.0 License

var x: boolean = (true ? 1 : "");
//~^ ERROR: Type 'number | string' is not assignable to type 'boolean'.