// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyDestructuringVarDeclaration.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

var [a], {b}, c, d; // error
//~^ ERROR: A destructuring declaration must have an initializer.
//~| ERROR: A destructuring declaration must have an initializer.
//~| ERROR: Binding element 'a' implicitly has an 'any' type.
//~| ERROR: Binding element 'a' implicitly has an 'any' type.
//~| ERROR: Binding element 'b' implicitly has an 'any' type.
//~| ERROR: Binding element 'b' implicitly has an 'any' type.

var [a1 = undefined], {b1 = null}, c1 = undefined, d1 = null; // error
//~^ ERROR: A destructuring declaration must have an initializer.
//~| ERROR: A destructuring declaration must have an initializer.

var [a2]: [any], {b2}: { b2: any }, c2: any, d2: any;
//~^ ERROR: A destructuring declaration must have an initializer.
//~| ERROR: A destructuring declaration must have an initializer.

var {b3}: { b3 }, c3: { b3 }; // error in type instead
//~^ ERROR: A destructuring declaration must have an initializer.
//~| ERROR: Member 'b3' implicitly has an 'any' type.
//~| ERROR: Member 'b3' implicitly has an 'any' type.

var [a4] = [undefined], {b4} = { b4: null }, c4 = undefined, d4 = null; // error

var [a5 = undefined] = []; // error

const [a6]
//~^ ERROR: Declarations must be initialized.
//~| ERROR: Binding element 'a6' implicitly has an 'any' type.
//~| ERROR: Binding element 'a6' implicitly has an 'any' type.