// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuringPropertyAssignmentNameIsNotAssignmentTarget.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

function qux(bar: { value: number }) {
    let foo: number;
    ({ value: foo } = bar);
    let x = () => bar;
}

