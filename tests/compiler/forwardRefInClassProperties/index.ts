// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/forwardRefInClassProperties.ts`, Apache-2.0 License

class Test
{
    _b = this._a; // undefined, no error/warning
    //~^ ERROR: Property '_a' is used before its initialization.
    _a = 3;

    static _B = Test._A; // undefined, no error/warning
    //~^ ERROR: Property '_A' is used before its initialization.
    static _A = 3;

    method()
    {
        let a = b; // Property 'b' is used before its initialization.
        //~^ ERROR: Block-scoped variable 'b' used before its declaration.
        let b = 3;
    }
}
