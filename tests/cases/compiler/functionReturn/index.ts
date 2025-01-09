// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionReturn.ts`, Apache-2.0 License


function f0(): void { }
function f1() {
    var n: any = f0();
}
function f2(): any { }
function f3(): string { return; }
function f4(): string {
    return '';
    return;
}
function f5(): string {
    return '';
    return undefined;
}