// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parseJsxExtends1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: jsx=react

declare const React: any;

export function Foo() {
    // No error; "const" is lowercase and therefore intrinsic.
    return <const T extends/>
}
