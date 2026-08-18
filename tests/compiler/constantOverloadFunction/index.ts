// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constantOverloadFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Base { foo() { } }
class Derived1 extends Base { bar() { } }
class Derived2 extends Base { baz() { } }
class Derived3 extends Base { biz() { } }

function foo(tagName: 'canvas'): Derived1;
function foo(tagName:  'div'): Derived2;
function foo(tagName: 'span'): Derived3;
function foo(tagName: string): Base;
function foo(tagName: any): Base {
    return null;
    //~^ ERROR: Type 'null' is not assignable to type 'Base'.
}
