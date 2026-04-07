// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/discriminatingUnionWithUnionPropertyAgainstUndefinedWithoutStrictNullChecks.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks=false
//@compiler-options: noEmit

interface A {}
interface B {}

declare let opts:
   | { objectRef?: undefined; getObjectRef: () => any }
   | { objectRef: A | B; getObjectRef?: undefined };

opts.objectRef || opts.getObjectRef();

// repro #49643 issuecomment-1174455723

interface X {
    foo: string;
}

interface Y {
    baz: number;
}

interface A2 {
    result: unknown;
    error: undefined;
}

interface B2 {
    error: X | Y;
}

const testMethod = (m: A2 | B2) => {
    if (m.error) {
        m; // should be A2 | B2
    } else {
        m; // should be A2 | B2
    }
}

