// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeParamExtendsOtherTypeParam.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A<T, U extends T> { }
class B<T extends Object, U extends T> {
    data: A<Object, Object>;
    //~^ ERROR: Property 'data' has no initializer and is not definitely assigned in the constructor.
}

// Below 2 should compile without error 
var x: A< { a: string }, { a: string; b: number }>;
var y: B< { a: string }, { a: string; b: number }>;


// Below should be in error
var x1: A<{ a: string;}, { b: string }>;
//~^ ERROR: Type '{ b: string; }' does not satisfy the constraint '{ a: string; }'.
var x2: A<{ a: string;}, { a: number }>;
//~^ ERROR: Type '{ a: number; }' does not satisfy the constraint '{ a: string; }'.
var x3: B<{ a: string;}, { b: string }>;
//~^ ERROR: Type '{ b: string; }' does not satisfy the constraint '{ a: string; }'.
var x4: B<{ a: string;}, { a: number }>;
//~^ ERROR: Type '{ a: number; }' does not satisfy the constraint '{ a: string; }'.
var x5: A<{ a: string; b: number }, { a: string }>;
//~^ ERROR: Type '{ a: string; }' does not satisfy the constraint '{ a: string; b: number; }'.
var x6: B<{ a: string; b: number }, { a: string }>;
//~^ ERROR: Type '{ a: string; }' does not satisfy the constraint '{ a: string; b: number; }'.

interface I1 {
    a: string;
}

interface I2 {
    a: string;
    b: number;
}

var x7: A<I2, I1>;
//~^ ERROR: Type 'I1' does not satisfy the constraint 'I2'.
var x8: B<I2, I1>;
//~^ ERROR: Type 'I1' does not satisfy the constraint 'I2'.