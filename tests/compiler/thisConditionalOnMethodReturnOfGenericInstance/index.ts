// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/thisConditionalOnMethodReturnOfGenericInstance.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

class A<T> {
    unmeasurableUsage!: {[K in keyof T]-?: T[K]};
}

class B<T> extends A<T> {
    method(): string | (this extends C ? undefined : null) {
        return "";
    }
}

class C<T = any> extends B<T> {
    marker!: string;
}

const x = new C<{}>();

const y = x.method(); // usage flags `method` in `B` as circular and marks `y` as the error-any type
