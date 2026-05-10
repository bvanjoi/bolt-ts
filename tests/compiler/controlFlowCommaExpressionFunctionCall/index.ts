// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowCommaExpressionFunctionCall.ts`, Apache-2.0 License

//@compiler-options: target=es2015

const otherValue = () => true;
const value : number | string = null as any;

function isNumber(obj: any): obj is number {
    return true; // method implementation irrelevant
}

// Bad case - fails
if (isNumber((otherValue(), value))) {
    const b = value; // string | number , but should be number
}