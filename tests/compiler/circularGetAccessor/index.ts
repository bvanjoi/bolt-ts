// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/circularGetAccessor.ts`, Apache-2.0 License

//compiler-options: target=es2015
//@[noImplicitAny=true]   compiler-options: noImplicitAny
//@[noImplicitAny=false]  compiler-options: noImplicitAny=false

declare class C {
    get foo(): typeof this.foo;
    //~^ ERROR: 'foo' is referenced directly or indirectly in its own type annotation.
}
