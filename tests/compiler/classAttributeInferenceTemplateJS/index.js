// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classAttributeInferenceTemplateJS.ts`, Apache-2.0 License

//@compiler-options: noEmit
//@compiler-options: target=esnext
//@compiler-options: checkJs
//@compiler-options: strict

class MyClass {
    property;
    property2;

    constructor() {
        const variable = 'something'

        this.property = `foo`; // Correctly inferred as `string`
        this.property2 = `foo-${variable}`; // Causes an error

        const localProperty = `foo-${variable}`; // Correctly inferred as `string`
    }
}

class MyClass2 {
    accessor property;
    accessor property2;

    constructor() {
        const variable = 'something'

        this.property = `foo`; // Correctly inferred as `string`
        this.property2 = `foo-${variable}`; // Causes an error

        const localProperty = `foo-${variable}`; // Correctly inferred as `string`
    }
}
