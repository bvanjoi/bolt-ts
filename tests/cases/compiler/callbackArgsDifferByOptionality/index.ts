// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/callbackArgsDifferByOptionality.ts`, Apache-2.0 License

function x3(callback: (x?: 'hi') => number);
function x3(callback: (x: string) => number);
function x3(callback: (x: any) => number) {
    cb();
    //~^ ERROR: Cannot find name 'cb'.
}