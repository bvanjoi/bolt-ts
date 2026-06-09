// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalSetterParam.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class foo {

    public set bar(param?:any) { }
    //~^ ERROR: A 'set' accessor cannot have an optional parameter.
}
