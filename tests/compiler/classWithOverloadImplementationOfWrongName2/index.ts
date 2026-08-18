// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classWithOverloadImplementationOfWrongName2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C {
    foo(): string;
    bar(x): any { }
    //~^ ERROR: Function implementation name must be 'foo'.
    foo(x): number;
    //~^ ERROR: Function implementation is missing or not immediately following the declaration.
}
