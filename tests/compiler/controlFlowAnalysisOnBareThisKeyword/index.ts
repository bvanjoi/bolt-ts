// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/controlFlowAnalysisOnBareThisKeyword.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

declare function isBig(x: any): x is { big: true };
function bigger(this: {}) {
    if (isBig(this)) {
        this.big; // Expect property to exist
        this.smaller;
        //~^ ERROR: Property 'smaller' does not exist on type '{ big: true; }'.
    }
}

function bar(this: string | number) {
    if (typeof this === "string") {
        const x: string = this;
    }
}