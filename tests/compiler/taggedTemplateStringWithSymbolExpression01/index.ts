// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/taggedTemplateStringWithSymbolExpression01.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

declare function foo(template: any, val: symbol): number;
let x!: symbol;

let result: number = foo`${x}`;
