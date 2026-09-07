// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classStaticPropertyTypeGuard.ts`, Apache-2.0 License

//@compiler-options: strictNullChecks
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

class A {
    private static _a: string | undefined;

    public get a(): string {
        if (A._a) {
            return A._a; // is possibly null or undefined.
        }
        return A._a = 'helloworld';
    }
}