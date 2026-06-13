// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/uncalledFunctionChecksInConditional.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

declare function isFoo(): boolean;
declare function isBar(): boolean;
declare const isUndefinedFoo: (() => boolean) | undefined;

if (isFoo) {
    // error on isFoo
    //~^^ ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
}

if (isFoo || isBar) {
    // error on isFoo, isBar
    //~^^ ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
    //~| ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
}

if (isFoo || isFoo()) {
    // error on isFoo
    //~^^ ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
}

if (isUndefinedFoo || isFoo()) {
    // no error
}

if (isFoo && isFoo()) {
    // no error
}

declare const x: boolean;
declare const ux: boolean | undefined;
declare const y: boolean;
declare const uy: boolean | undefined;
declare function z(): boolean;
declare const uz: (() => boolean) | undefined;

if (x || isFoo) {
    // error on isFoo
    //~^^ ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
}

if (isFoo || x) {
    // error on isFoo
    //~^^ ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
}

if (x || y || z() || isFoo) {
    // error on isFoo
    //~^^ ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
}

if (x || uy || z || isUndefinedFoo) {
    // error on z
    //~^^ ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
}

if (ux || y || uz || isFoo) {
    // error on isFoo
    //~^^ ERROR: This condition will always return true since this function is always defined. Did you mean to call it instead?
}

if (x && z) {
    // no error
    z();
}