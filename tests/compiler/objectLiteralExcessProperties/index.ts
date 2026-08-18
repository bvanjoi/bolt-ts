// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLiteralExcessProperties.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Book {
    foreword: string;
}

interface Cover {
    color?: string;
}

var b1: Book = { forword: "oops" };
//~^ ERROR: Object literal may only specify known properties, and 'forword' does not exist in type 'Book'.

var b2: Book | string = { foreward: "nope" };
//~^ ERROR: Object literal may only specify known properties, and 'foreward' does not exist in type 'Book'.

var b3: Book | (Book[]) = [{ foreword: "hello" }, { forwards: "back" }];
//~^ ERROR: Object literal may only specify known properties, and 'forwards' does not exist in type 'Book'.

var b4: Book & Cover = { foreword: "hi", colour: "blue" };
//~^ ERROR: Object literal may only specify known properties, and 'colour' does not exist in type 'Book & Cover'.

var b5: Book & Cover = { foreward: "hi", color: "blue" };
//~^ ERROR: Object literal may only specify known properties, and 'foreward' does not exist in type 'Book & Cover'.

var b6: Book & Cover = { foreword: "hi", color: "blue", price: 10.99 };
//~^ ERROR: Object literal may only specify known properties, and 'price' does not exist in type 'Book & Cover'.

var b7: Book & number = { foreword: "hi", price: 10.99 };
//~^ ERROR: Type '{ foreword: string; price: number; }' is not assignable to type 'Book & number'.

var b8: Cover | Cover[] = { couleur : "non" };
//~^ ERROR: Object literal may only specify known properties, and 'couleur' does not exist in type 'Cover | Cover[]'.
  
var b9: Book | Book[] = { forewarned: "still no" };
//~^ ERROR: Object literal may only specify known properties, and 'forewarned' does not exist in type 'Book | Book[]'.

interface Indexed {
    [n: number]: Cover;
}

var b10: Indexed = { 0: { }, '1': { } }; // ok

var b11: Indexed = { 0: { colour: "blue" } }; // nested object literal still errors
//~^ ERROR: Object literal may only specify known properties, and 'colour' does not exist in type 'Cover'.

// Repros inspired by #28752

function test<T extends IFoo>() {
  //~^ ERROR: Cannot find name 'IFoo'.
    // No excess property checks on generic types
    const obj1: T = { name: "test" };
    //~^ ERROR: Type '{ name: string; }' is not assignable to type 'T'.
    // No excess property checks on intersections involving generics
    const obj2: T & { prop: boolean } = { name: "test", prop: true };
    //~^ ERROR: Type '{ name: string; prop: boolean; }' is not assignable to type 'T & { prop: boolean; }'.
    // Excess property checks only on non-generic parts of unions
    const obj3: T | { prop: boolean } = { name: "test", prop: true };
    //~^ ERROR: Object literal may only specify known properties, and 'name' does not exist in type '{ prop: boolean; }'.
    // Excess property checks only on non-generic parts of unions
    const obj4: T & { prop: boolean } | { name: string } = { name: "test", prop: true };
    //~^ ERROR: Object literal may only specify known properties, and 'prop' does not exist in type '{ name: string; }'.
    // No excess property checks when union includes 'object' type
    const obj5: object | { x: string } = { z: 'abc' }
    // The 'object' type has no effect on intersections
    const obj6: object & { x: string } = { z: 'abc' }
    //~^ ERROR: Object literal may only specify known properties, and 'z' does not exist in type 'object & { x: string; }'.
}
