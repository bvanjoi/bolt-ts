// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/accessInstanceMemberFromStaticMethod01.ts`, Apache-2.0 License

class C {
    static foo: string;

    bar() {
        let k = foo;
        //~^ ERROR: Cannot find name 'foo'.
    }
}
