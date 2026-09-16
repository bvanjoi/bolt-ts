// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/ambientModules.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@compiler-options: target=es2015
//@run-fail

declare namespace Foo.Bar { export var foo; };
Foo.Bar.foo = 5; 