// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/innerTypeCheckOfLambdaArgument.ts`, Apache-2.0 License
function takesCallback(callback: (n) =>any) {

}
 
takesCallback(
 
	function inner(n) {
                // this line should raise an error
                // otherwise, there's a bug in overload resolution / partial typechecking
		var k: string = 10; 
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
    }
);
