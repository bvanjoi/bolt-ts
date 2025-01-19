// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/typeCheckingInsideFunctionExpressionInArray.ts`, Apache-2.0 License

var functions = [function () {
  var k: string = 10;
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
    k = new Object();
    //~^ ERROR: Type 'Object' is not assignable to type 'string'.
    [1, 2, 3].NonexistantMethod();
    //~^ ERROR: Property 'NonexistantMethod' does not exist on type 'number[]'.
    derp();
    //~^ ERROR: Cannot find name 'derp'.
}];
