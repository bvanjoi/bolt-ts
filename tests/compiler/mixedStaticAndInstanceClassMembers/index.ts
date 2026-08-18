// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mixedStaticAndInstanceClassMembers.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A {
    f() {}
    static m1 (a: string): void;
    m1 (a: number): void;
    //~^ ERROR: Function overload must be static.
    m1 (a: any): void {
    }
}

class B {
    f() {}
    m1 (a: string): void;
    static m1 (a: number): void;
    //~^ ERROR: Function overload must not be static.
    m1 (a: any): void {
    //~^ ERROR: Function overload must be static.
    }
}