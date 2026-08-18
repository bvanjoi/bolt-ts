// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadOnConstConstraintChecks1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Base { foo() { } }
class Derived1 extends Base { bar() { } }
class Derived2 extends Base { baz() { } }
class Derived3 extends Base { biz() { } }

interface MyDoc { // Document
    createElement(tagName: string): Base;
    createElement(tagName: 'canvas'): Derived1;
    createElement(tagName: 'div'): Derived2;
    createElement(tagName: 'span'): Derived3;
    // + 100 more
}

class D implements MyDoc {
    createElement(tagName:string): Base;
    createElement(tagName: 'canvas'): Derived1;
    createElement(tagName: 'div'): Derived2;
    createElement(tagName: 'span'): Derived3;
    createElement(tagName:any): Base {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'Base'.
    }
}