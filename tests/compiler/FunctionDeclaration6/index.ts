// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/FunctionDeclaration6.ts`, Apache-2.0 License

//@compiler-options: target=es2015

{
    function foo();
    //~^ ERROR: 'foo', which lacks return-type annotation, implicitly has an 'any' return type.
    function bar() { }
    //~^ ERROR: Function implementation name must be 'foo'.
}