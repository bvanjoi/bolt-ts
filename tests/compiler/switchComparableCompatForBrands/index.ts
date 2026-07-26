// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/switchComparableCompatForBrands.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class MyBrand
{
    private _a: number;
    //~^ ERROR: Property '_a' has no initializer and is not definitely assigned in the constructor.
}

function test(strInput: string & MyBrand) {
    switch(strInput)
    {
        case "a":
        return 1;
    }
    return 0;
}
