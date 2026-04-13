// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/indexIntoArraySubclass.ts`, Apache-2.0 License

//@compiler-options: target=es2015
interface Foo2<T> extends Array<T> { }
declare var x2: Foo2<string>;
var r = x2[0]; // string
r = 0; //error
//~^ ERROR: Type 'number' is not assignable to type 'string'.