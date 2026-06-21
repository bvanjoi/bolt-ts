// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/spreadBooleanRespectsFreshness.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

type Foo = FooBase | FooArray;
type FooBase = string | false;
type FooArray = FooBase[];

declare let foo1: Foo;
declare let foo2: Foo;
foo1 = [...Array.isArray(foo2) ? foo2 : [foo2]];