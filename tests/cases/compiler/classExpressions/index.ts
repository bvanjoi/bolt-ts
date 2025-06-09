// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/classExpressions.ts`, Apache-2.0 License

interface A {}
let x = class B implements A {
    prop: number;
    onStart(): void {
    }
    func = () => {
    }
};



(class implements A {})