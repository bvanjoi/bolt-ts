// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedPrivateVariableInClass1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

class greeter {
    private x: string;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
    //~| ERROR: 'x' is declared but its value is never read.
}
