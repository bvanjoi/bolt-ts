// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unionOfEnumInference.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

enum Enum { A, B, C }

interface Interface<T extends Enum> {
	type: T;
}

function foo<T extends Enum>(x: Interface<T>) { }

function bar(x: Interface<Enum.A | Enum.B> | Interface<Enum.C>) {
	foo(x);
}