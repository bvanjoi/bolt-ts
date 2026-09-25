// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedPrivateVariableInClass5.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

class greeter {
    private x!: string;
    private y!: string;
    //~^ ERROR: 'y' is declared but its value is never read.
    public  z!: string;

    constructor() {
        this.x;
    }
}