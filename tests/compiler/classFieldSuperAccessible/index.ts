// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classFieldSuperAccessible.ts`, Apache-2.0 License

//@compiler-options: target=esnext

class A extends class Expr {} {
    static {
        console.log(super.name);
    }
}
class B extends Number {
    static {
        console.log(super.EPSILON);
    }
}
class C extends Array {
    foo() {
        console.log(super.length);
    }
}

class D {
    accessor b = () => {}
}
class E extends D {
    foo() {
        super.b()
    }
}