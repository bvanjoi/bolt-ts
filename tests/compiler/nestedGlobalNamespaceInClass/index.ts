// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nestedGlobalNamespaceInClass.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
    global x
    //~^ ERROR: Unexpected keyword or identifier.
    //~| ERROR: Member 'global' implicitly has an 'any' type.
    //~| ERROR: Member 'x' implicitly has an 'any' type.
}
