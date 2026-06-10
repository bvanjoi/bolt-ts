// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/definiteAssignmentOfDestructuredVariable.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

interface Options {
    a?: number | object;
    b: () => void;
}

class C<T extends Options> {
    foo!: { [P in keyof T]: T[P] }

    method() {
        let { a, b } = this.foo;
        !(a && b);
        a;
    }
}