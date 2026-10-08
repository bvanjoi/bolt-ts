// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleDuplicateIdentifiers.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export var Foo = 2;
//~^ ERROR: Cannot redeclare exported variable 'default'.
export var Foo = 42; // Should error
//~^ ERROR: Cannot redeclare exported variable 'default'.

export interface Bar {
	_brand1: any;
}

export interface Bar { // Shouldn't error
	_brand2: any;
}

export namespace FooBar {
	export var member1 = 2;
}

export namespace FooBar { // Shouldn't error
	export var member2 = 42;
}

export class Kettle {
	member1 = 2;
}

export class Kettle { // Should error
  //~^ ERROR: Duplicate identifier 'Kettle'.
	member2 = 42;
}

export var Pot = 2;
Pot = 42; // Shouldn't error

export enum Utensils {
	Spoon,
	Fork,
	Knife
}

export enum Utensils { // Shouldn't error
	Spork = 3
}
