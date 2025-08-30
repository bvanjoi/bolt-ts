// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/super.ts`, Apache-2.0 License

class Base {
    constructor() {
        var x;
    }
    public foo() {
        return "base";
    }

    public bar() {
        return "basebar";
    }
}

class Sub1 extends Base {
    public foo() {
        let s0: string = super.foo();
        let s1: string = super.bar();
        return "sub1" + super.foo() + super.bar();
    }
}


class SubSub1 extends Sub1 {
    public foo() {
        let s0: string = super.foo();
        let s1: string = super.bar();
        return "subsub1" + super.foo();
    }
}

class Base2 {
    public foo() {
        super.foo();
        //~^ ERROR: 'super' can only be referenced in a derived class.
    }
}

var s = new Sub1();
var ss = new SubSub1();
let sss: string = s.foo() + ss.foo();

