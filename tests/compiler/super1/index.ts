// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/super1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

// Case 1
class Base1 {
    public foo() {
        return "base";
    }
}

class Sub1 extends Base1 {
    public bar() {
        return "base";
    }
}

class SubSub1 extends Sub1 {
    public bar() {
        return super.super.foo;
        //~^ ERROR: Property 'super' does not exist on type 'Sub1<SubSub1>'.
    }
}

// Case 2
class Base2 {
    public foo() {
        return "base";
    }
}

class SubE2 extends Base2 {
    public bar() {
        return super.prototype.foo = null;
        //~^ ERROR: Property 'prototype' does not exist on type 'Base2<SubE2>'.
    }
}

// Case 3
class Base3 {
    public foo() {
        return "base";
    }
}

class SubE3 extends Base3 {
    public bar() {
        return super.bar();
        //~^ ERROR: Property 'bar' does not exist on type 'Base3<SubE3>'.
    }
}

// Case 4
namespace Base4 {
    class Sub4 {
        public x(){
            return "hello";
        }
    }
    
    export class SubSub4 extends Sub4{
        public x(){
            return super.x();
        }
    }
    
    export class Sub4E {
        public x() {
            return super.x();
            //~^ ERROR: 'super' can only be referenced in a derived class.
        }
    }
}
