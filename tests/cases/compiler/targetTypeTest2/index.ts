// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/targetTypeTest2.ts`, Apache-2.0 License

var a : any[] = [1,2,"3"];


function func1(stuff:any[]) { return stuff; }

function func2(stuff1:string, stuff2:number, stuff3:number) {
	return func1([stuff1, stuff2, stuff3]);
}