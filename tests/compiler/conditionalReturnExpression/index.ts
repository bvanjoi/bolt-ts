// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/conditionalReturnExpression.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: noEmit

function return1(x: boolean): 3 {
    return (x ? (1) : 2);
    //~^ ERROR: Type '1' is not assignable to type '3'.
    //~| ERROR: Type '2' is not assignable to type '3'.
}

declare function getAny(): any;

function return2(x: string): string {
    return x.startsWith("a") ? getAny() : 1;
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
}

function return3(x: string): string {
    return x.startsWith("a") ? "a" : x;
}

function return4(x: string): string {
    return (x.startsWith("a") ? getAny() : 1) as string;
}

const return5 = (x: string): string => x.startsWith("a") ? getAny() : 1;
    //~^ ERROR: Type 'number' is not assignable to type 'string'.

const return6 = (x: string): string => (x.startsWith("a") ? getAny() : 1) as string;