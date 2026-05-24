// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowCommaOperatorNestedWithinLHS.ts`, Apache-2.0 License

//@compiler-options: target=es2015

const otherValue = () => true;
const value: { inner: number | string } = null as any;

function isNumber(obj: any): obj is number {
    return true; // method implementation irrelevant
}

if (typeof (otherValue(), value).inner === 'number') {
    const a = value.inner; // number
    const a1: 42 = value.inner; // number, but should be 42
    //~^ ERROR: Type 'number' is not assignable to type '42'.
    const b: number = (otherValue(), value).inner; // string | number , but should be number
    const b1: 42 = (otherValue(), value).inner; // string | number , but should be 42
    //~^ ERROR: Type 'number' is not assignable to type '42'.
}

if (isNumber((otherValue(), value).inner)) {
    const a = value.inner; // number
    const a1: 42 = value.inner; // number, but should be 42
    //~^ ERROR: Type 'number' is not assignable to type '42'.
    const b: number = (otherValue(), value).inner; // string | number , but should be number
    const b1: 42 = (otherValue(), value).inner; // string | number , but should be 42
    //~^ ERROR: Type 'number' is not assignable to type '42'.
}