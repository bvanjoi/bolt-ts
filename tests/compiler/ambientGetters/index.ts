// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/ambientGetters.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

declare class A {
    get length() : number;
}

declare class B {
    get length() { return 0; }
    //~^ ERROR: An implementation cannot be declared in type contexts.
}