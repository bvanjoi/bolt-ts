// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/callbackArgsDifferByOptionality.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function x3(callback: (x?: 'hi') => number);
function x3(callback: (x: string) => number);
function x3(callback: (x: any) => number) {
    cb();
    //~^ ERROR: Cannot find name 'cb'.
}
