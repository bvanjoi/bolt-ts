// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/spyComparisonChecking.ts`, Apache-2.0 License

//@compiler-options: target=es2015
interface Spy {
    (...params: any[]): any;

    identity: string;
    and: Function;
    mostRecentCall: { args: any[]; };
    argsForCall: any[];
}

type SpyObj<T> = T & {
    [k in keyof T]: Spy;
}

declare function createSpyObj<T>(
    name: string, names: Array<keyof T>): SpyObj<T>;

function mock<T>(spyName: string, methodNames: Array<keyof T>): SpyObj<T> {
    const spyObj = createSpyObj<T>(spyName, methodNames);
    for (const methodName of methodNames) {
        spyObj[methodName].and.returnValue(1);
        //~^ ERROR: Property 'returnValue' does not exist on type 'Function'.
    }
    return spyObj;
}