// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/extractInferenceImprovement.ts`, Apache-2.0 License

//@compiler-options: target=es6


function getProperty2<T, K extends keyof T>(obj: T, key: Extract<K, string>): T[K] {
    return obj[key];
}

function getProperty3<T, K extends Extract<keyof T, string>>(obj: T, key: K): T[K] {
    return obj[key];
}

const s = Symbol();
interface StrNum {
    first: string;
    second: number;
    [s]: string;
}
const obj: StrNum = {} as any;

let prop: string;

// should work
prop = getProperty2(obj, 'first');

prop = getProperty3(obj, 'first');

// Should fail
prop = getProperty2(obj, s);
//~^ ERROR: Argument of type 'unique symbol' is not assignable to parameter of type 'never'.

prop = getProperty3(obj, s);
//~^ ERROR: Argument of type 'unique symbol' is not assignable to parameter of type '"first" | "second"'.
//~| ERROR: Type 'number | string' is not assignable to type 'string'.

const d: string = getProperty3(obj, 'second');
//~^ ERROR: Type 'number' is not assignable to type 'string'.