// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/deleteOperator1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var a;
var x: boolean = delete a;
//~^ ERROR: 'delete' cannot be called on an identifier in strict mode.
//~| ERROR: The operand of a 'delete' operator must be a property reference.
var y: any = delete a;
//~^ ERROR: 'delete' cannot be called on an identifier in strict mode.
//~| ERROR: The operand of a 'delete' operator must be a property reference.
var z: number = delete a;
//~^ ERROR: 'delete' cannot be called on an identifier in strict mode.
//~| ERROR: The operand of a 'delete' operator must be a property reference.
//~| ERROR: Type 'boolean' is not assignable to type 'number'.