// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/numberOnLeftSideOfInExpression.ts`, Apache-2.0 License

//@ run-fail

var left: number;
var right: any;
left in right;