// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/superCallWithMissingBaseClass.ts`, Apache-2.0 License

class Foo extends Bar {   //~ ERROR: Cannot find name 'Bar'.
    m1() {
        return super.m1();
    }

    static m2() {
        return super.m2();
    }
}