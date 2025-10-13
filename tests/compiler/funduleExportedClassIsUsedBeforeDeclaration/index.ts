// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/funduleExportedClassIsUsedBeforeDeclaration.ts`, Apache-2.0 License

//@ run-fail

interface A {   // interface before module declaration
    (): B.C;    // uses defined below class in module
}
declare function B(): B.C;  // function merged with module
declare namespace B {
    export class C {    // class defined in module 
    }
}
new B.C(); 