// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/scopeCheckStaticInitializer.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class X {
    static illegalBeforeProperty = X.data;
    //~^ ERROR: Property 'data' is used before its initialization.
    static okBeforeMethod = X.method;

    static illegal2 = After.data;
    //~^ ERROR: Class 'After' used before its declaration.
    //~| ERROR: Property 'data' is used before its initialization.
    static illegal3 = After.method;
    //~^ ERROR: Class 'After' used before its declaration.
    static data = 13;
    static method() { }
}
class After {
    static data = 12;
    static method() { };
}

