// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/errorRecoveryInClassDeclaration.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
    public bar() {
        var v = foo(  //~ ERROR: Cannot find name 'foo'.
            public blaz() {}
            //~^ ERROR: Expected ','.
            //~| ERROR: Expected ','.
            //~| ERROR: Cannot find name 'public'.
            //~| ERROR: Cannot find name 'blaz'.
            );
    }
}
