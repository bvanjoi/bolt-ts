// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/checkSwitchStatementIfCaseTypeIsString.ts`, Apache-2.0 License

declare function use(a: any): void;

class A {
    doIt(x: Array<string>): void {
        x.forEach((v) => {
            switch(v) {
                case "test": use(this);
            }
        });
    }
}