// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/targetTypeTest3.ts`, Apache-2.0 License


var a : string[] = [1,2,"3"]; // should produce an error
//~^ ERROR: Type 'number' is not assignable to type 'string'.
//~| ERROR: Type 'number' is not assignable to type 'string'.


function func1(stuff:any[]) { return stuff; }

function func2(stuff1:string, stuff2:number, stuff3:number) {
	return func1([stuff1, stuff2, stuff3]);
}