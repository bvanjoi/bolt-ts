// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/staticClassProps.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C
{
    public foo() {
        static z = 1;
        //~^ ERROR: Declaration or statement expected.
        //~| ERROR: '}' expected.
    }
}
//~^ ERROR: Declaration or statement expected.
