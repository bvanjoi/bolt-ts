// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/doNotWidenAtObjectLiteralPropertyAssignment.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

interface ITestEventInterval {
    begin: number;
}

interface IIntervalTreeNode {
    interval: ITestEventInterval;
    children?: IIntervalTreeNode[];
}

var test: IIntervalTreeNode[] = [{ interval: { begin: 0 }, children: null }]; // was error here because best common type is {}
//~^ ERROR: Type 'null' is not assignable to type 'undefined | IIntervalTreeNode[]'.
