// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleCodegenTest4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export namespace Baz { export var x = "hello"; }

Baz.x = "goodbye";
void 0;
