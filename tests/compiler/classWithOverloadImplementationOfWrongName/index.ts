// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classWithOverloadImplementationOfWrongName.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C {
    foo(): string;
    foo(x): number;
    bar(x): any { }
    //~^ ERROR: Function implementation name must be 'foo'.
}