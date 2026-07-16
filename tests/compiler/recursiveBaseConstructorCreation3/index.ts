// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveBaseConstructorCreation3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare class base<T> {
}
declare class abc<T> extends base<T> {
    foo: xyz;
}
declare class xyz extends abc {
  //~^ ERROR: Generic type 'abc<T>' requires 1 type argument.
}

var bar = new xyz(); // Error: Invalid 'new' expression.
//~^ ERROR: This expression is not constructable.
var r: xyz = bar.foo;