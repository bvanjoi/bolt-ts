// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/instanceofOperator.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: module=commonjs

// Spec:
// The instanceof operator requires the left operand to be of type Any or an object type, and the right 
// operand to be of type Any or a subtype of the ‘Function’ interface type. The result is always of the 
// Boolean primitive type.

namespace test {
    class Object { }
    //~^ ERROR: Class name cannot be 'Object' when targeting ES5 and above with module CommonJS.
    declare var obj: Object;



    4 instanceof null;
    //~^ ERROR: The left-hand side of an 'instanceof' expression must be of type 'any', an object type or a type parameter.
    //~| ERROR: The right-hand side of an 'instanceof' expression must be either of type 'any', a class, function, or other type assignable to the 'Function' interface type, or an object type with a 'Symbol.hasInstance' method.

    // Error and should be error
    obj instanceof 4;
    //~^ ERROR: The right-hand side of an 'instanceof' expression must be either of type 'any', a class, function, or other type assignable to the 'Function' interface type, or an object type with a 'Symbol.hasInstance' method.
    Object instanceof obj;
    //~^ ERROR: The right-hand side of an 'instanceof' expression must be either of type 'any', a class, function, or other type assignable to the 'Function' interface type, or an object type with a 'Symbol.hasInstance' method.

    // Error on left hand side
    null instanceof null;
    //~^ ERROR: The left-hand side of an 'instanceof' expression must be of type 'any', an object type or a type parameter.
    //~| ERROR: The right-hand side of an 'instanceof' expression must be either of type 'any', a class, function, or other type assignable to the 'Function' interface type, or an object type with a 'Symbol.hasInstance' method.
    obj instanceof Object;
    undefined instanceof undefined;
    //~^ ERROR: The left-hand side of an 'instanceof' expression must be of type 'any', an object type or a type parameter.
    //~| ERROR: The right-hand side of an 'instanceof' expression must be either of type 'any', a class, function, or other type assignable to the 'Function' interface type, or an object type with a 'Symbol.hasInstance' method.
}