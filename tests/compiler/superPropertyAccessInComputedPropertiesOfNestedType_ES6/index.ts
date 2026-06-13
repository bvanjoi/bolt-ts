// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superPropertyAccessInComputedPropertiesOfNestedType_ES6.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A {
    foo() { return 1; }
}

class B extends A {
    foo() { return 2; }
    bar() {
        return class {
            [super.foo()]() {
                return 100;
            }
        }
    }
}
