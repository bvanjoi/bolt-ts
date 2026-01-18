// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classDeclarationCheckUsedBeforeDefinitionInItself.ts`, Apache-2.0 License

//@compiler-options: target=es6

class C3 { 
    static intance = new C3();  // ok
}