// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/emitAccessExpressionOfCastedObjectLiteralExpressionInArrowFunctionES5.ts`, Apache-2.0 License

//@[target=es5]     compiler-options: target=es5
//@[target=es2015]  compiler-options: target=es2015
//@compiler-options: strict=false

(x) => ({ "1": "one", "2": "two" } as { [key: string]: string })[x];
(x) => ({ "1": "one", "2": "two" } as { [key: string]: string }).x;