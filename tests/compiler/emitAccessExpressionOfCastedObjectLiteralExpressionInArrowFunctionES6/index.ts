// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/emitAccessExpressionOfCastedObjectLiteralExpressionInArrowFunctionES6.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: strict=false

(x) => ({ "1": "one", "2": "two" } as { [key: string]: string })[x];
(x) => ({ "1": "one", "2": "two" } as { [key: string]: string }).x;