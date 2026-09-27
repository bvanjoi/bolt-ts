// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedPrivateStaticMembers.ts`, Apache-2.0 License

//@compiler-options: noUnusedLocals
//@compiler-options: target=esnext
//@compiler-options: noUnusedParameters

class Test1 {
    private static m1() {}
    public static test() {
        Test1.m1();
    }
}

class Test2 {
    private static p1 = 0
    public static test() {
        Test2.p1;
    }
}

class Test3 {
    private static p1 = 0;
    //~^ ERROR: 'p1' is declared but its value is never read.
    private static m1() {}
    //~^ ERROR: 'm1' is declared but its value is never read.
}

class Test4 {
    private static m1(n: number): number {
    //~^ ERROR: 'm1' is declared but its value is never read.
        return (n === 0) ? 1 : (n * Test4.m1(n - 1));
    }

    private static m2(n: number): number {
    //~^ ERROR: 'm2' is declared but its value is never read.
        return (n === 0) ? 1 : (n * Test4["m2"](n - 1));
    }
}

class Test5 {
    private static m1() {}
    public static test() {
        Test5["m1"]();
    }
}

class Test6 {
    private static p1 = 0;
    public static test() {
        Test6["p1"];
    }
}
