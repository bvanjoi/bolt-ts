// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/undefinedSymbolReferencedInArrayLiteral1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var tokens = [{ startIndex: deltaOffset }];
//~^ ERROR: Cannot find name 'deltaOffset'.

var functions = [function() {
    [1, 2, 3].NonexistantMethod();
    //~^ ERROR: Property 'NonexistantMethod' does not exist on type 'number[]'.
    anotherNonExistingMethod();
    //~^ ERROR: Cannot find name 'anotherNonExistingMethod'.
}];

