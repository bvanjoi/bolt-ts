// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/undefinedTypeAssignment4.ts`, Apache-2.0 License

class undefined { //~ERROR: Class name cannot be 'undefined'.
	foo: string;
}
interface undefined { //~ ERROR: Interface name cannot be 'undefined'.
	member: number;
}
namespace undefined { //~ ERROR: Declaration name conflicts with built-in global identifier 'undefined'.
	export var x = 42;
}
var x: undefined;
var y: typeof undefined;
