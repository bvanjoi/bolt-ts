// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentToObjectAndFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var errObj: Object = { toString: 0 }; // Error, incompatible toString
//~^ ERROR: Type 'number' is not assignable to type '() => string'.
var goodObj: Object = {
    toString(x?) {
        return "";
    }
}; // Ok, because toString is a subtype of Object's toString

var errFun: Function = {}; // Error for no call signature
//~^ ERROR: Type '{ }' is missing the following properties from type 'Function': apply, call, and 7 more.

function foo() { }
namespace foo {
    export var boom = 0;
}

var goodFundule: Function = foo; // ok

function bar() { }
namespace bar {
    export function apply(thisArg: string, argArray?: string) { }
}

var goodFundule2: Function = bar; // ok

function bad() { }
namespace bad {
    export var apply = 0;
}

var badFundule: Function = bad; // error
//~^ ERROR: Type 'typeof bad' is not assignable to type 'Function'.