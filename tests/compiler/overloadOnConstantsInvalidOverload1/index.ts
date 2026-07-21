// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadOnConstantsInvalidOverload1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Base { foo() { } }
class Derived1 extends Base { bar() { } }
class Derived2 extends Base { baz() { } }
class Derived3 extends Base { biz() { } }

function foo(name: "SPAN"): Derived1;
//~^ ERROR: This overload signature is not compatible with its implementation signature.
function foo(name: "DIV"): Derived2 {
    return null;
    //~^ ERROR: Type 'null' is not assignable to type 'Derived2'.
}

foo("HI");
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type '"SPAN"'.
