// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionReturn.ts`, Apache-2.0 License


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

function f6(value: number) {
    switch (value) {
        case 1:
            return 1;
        default:
            return 0;
    }
    return 2;
}

const v0: 1 = f6(2);
//~^ ERROR: Type 'number' is not assignable to type '1'.
const v2: 0 | 1 | 2 = f6(3);
