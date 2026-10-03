// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveSpecializationOfExtendedTypeWithError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es5]

interface HTMLSelectElement {
    options: HTMLSelectElement;
    <A>(name: A): any;
}
 
