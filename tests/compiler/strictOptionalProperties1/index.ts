// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/strictOptionalProperties1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: exactOptionalPropertyTypes
//@compiler-options: declaration

function f1(obj: { a?: string, b?: string | undefined }) {
    let a = obj.a;  // string | undefined
    let b = obj.b;  // string | undefined
    obj.a = 'hello';
    obj.b = 'hello';
    obj.a = undefined;  // Error
    //~^ ERROR: Type 'undefined' is not assignable to type 'string'.
    obj.b = undefined;
}

function f2(obj: { a?: string, b?: string | undefined }) {
    obj = obj;
    obj.a = obj.a;  // Error
    //~^ ERROR: Type 'undefined | string' is not assignable to type 'string'.
    obj.b = obj.b;
    if ('a' in obj) {
        obj.a;
        obj.a = obj.a;
    }
    else {
        obj.a;
        obj.a = obj.a;  // Error
        //~^ ERROR: Type 'undefined' is not assignable to type 'string'.
    }
    if (obj.hasOwnProperty('a')) {
        obj.a;
        obj.a = obj.a;
    }
    else {
        obj.a;
        obj.a = obj.a;  // Error
        //~^ ERROR: Type 'undefined' is not assignable to type 'string'.
    }
    if ('b' in obj) {
        obj.b;
        obj.b = obj.b;
    }
    else {
        obj.b;
        obj.b = obj.b;
    }
    if (obj.hasOwnProperty('b')) {
        obj.b;
        obj.b = obj.b;
    }
    else {
        obj.b;
        obj.b = obj.b;
    }
}

function f3(obj: Partial<{ a: string, b: string | undefined }>) {
    let a = obj.a;  // string | undefined
    let b = obj.b;  // string | undefined
    obj.a = 'hello';
    obj.b = 'hello';
    obj.a = undefined;  // Error
    //~^ ERROR: Type 'undefined' is not assignable to type 'string'.
    obj.b = undefined;
}

function f4(t: [string?]) {
    let x = t[0];  // string | undefined
    t[0] = 'hello';
    t[0] = undefined;  // Error
    //~^ ERROR: Type 'undefined' is not assignable to type 'string'.
}

function f4a(t1: [number, string?], t2: [number, string?, string?]) {
    t1 = t2;  // Wasn't an error, but should be
    //~^ ERROR: Type '[number, undefined | string?, undefined | string?]' is not assignable to type '[number, undefined | string?]'.
}

function f5(t: [number, string?, boolean?]) {
    t = [42];
    t = [42, 'abc'];
    t = [42, 'abc', true];
    t = [42, ,];
    t = [42, , ,];
    t = [42, , , ,];  // Error
    //~^ ERROR: Type '[number, undefined?, undefined?, undefined?]' is not assignable to type '[number, undefined | string?, undefined | false | true?]'.
    t = [, , true];  // Error
    //~^ ERROR: Type 'undefined' is not assignable to type 'number'.
    t = [42, undefined, true];  // Error
    //~^ ERROR: Type '[number, undefined, true]' is not assignable to type '[number, undefined | string?, undefined | false | true?]'.
}

function f6() {
    const t1 = [1, 2] as const;
    const t2 = [1, 2, ,] as const;
    const t3 = [1, 2, , ,] as const;
    const t4 = [1, , 2] as const;
    const t5 = [1, , , 2] as const;
}

// Example from #13195

type Props = {
    foo: string;
    bar: string
}

type InputProps = {
    foo?: string;
    bar: string;
}

const defaultProps: Pick<Props, 'foo'> = { foo: 'foo' };
const inputProps: InputProps = { foo: undefined, bar: 'bar' };
//~^ ERROR: Type '{ foo: undefined; bar: string; }' is not assignable to type 'InputProps'.
const completeProps: Props = { ...defaultProps, ...inputProps };

// Example from #13195

const t1: [number, string?, boolean?] = [1];
const t2: [number, string?, boolean?] = [1, undefined];
//~^ ERROR: Type '[number, undefined]' is not assignable to type '[number, undefined | string?, undefined | false | true?]'.
const t3: [number, string?, boolean?] = [1, "string", undefined];
//~^ ERROR: Type '[number, string, undefined]' is not assignable to type '[number, undefined | string?, undefined | false | true?]'.
const t4: [number, string?, boolean?] = [1, undefined, undefined];
//~^ ERROR: Type '[number, undefined, undefined]' is not assignable to type '[number, undefined | string?, undefined | false | true?]'.

// Example from #13195

const x: { foo?: number } = { foo: undefined };
//~^ ERROR: Type '{ foo: undefined; }' is not assignable to type '{ foo: undefined | number; }'.
const y: { foo: number } = { foo: 123, ...x };

// Index signatures and strict optional properties

interface Test {
    [key: string]: string;
    foo?: string;  // Should be ok
    bar?: string | undefined;  // Error
    //~^ ERROR: Property 'bar' of type 'undefined | string' is not assignable to 'string' index type 'string'.
}

// Strict optional properties and inference

declare let ox1: { p: string };
declare let ox2: { p: string | undefined };
declare let ox3: { p?: string };
declare let ox4: { p?: string | undefined };

declare let tx1: [string];
declare let tx2: [string | undefined];
declare let tx3: [string?];
declare let tx4: [(string | undefined)?];

declare function f11<T>(x: { p?: T }): T;

f11(ox1);  // string
f11(ox2);  // string | undefined
f11(ox3);  // string
f11(ox4);  // string | undefined

declare function f12<T>(x: [T?]): T;

f12(tx1);  // string
f12(tx2);  // string | undefined
f12(tx3);  // string
f12(tx4);  // string | undefined

declare function f13<T>(x: Partial<T>): T;

f13(ox1);  // { p: string }
f13(ox2);  // { p: string | undefined }
f13(ox3);  // { p: string }
f13(ox4);  // { p: string | undefined }

f13(tx1);  // [string]
f13(tx2);  // [string | undefined]
f13(tx3);  // [string]
f13(tx4);  // [string | undefined]

// Repro from #44388

type Undefinable<T> = T | undefined;

function expectNotUndefined<T>(value: Undefinable<T>): T {
    if (value === undefined) {
        throw new TypeError('value is undefined');
    }
    return value;
}

interface Bar {
    bar?: number;
}

function aa(input: Bar): void {
    const notUndefinedVal = expectNotUndefined(input.bar);
    bb(notUndefinedVal);
}

declare function bb(input: number): void;

interface U1 {
    name: string
    email?: string | number | undefined
}
interface U2 {
    name: string
    email?: string | number
}
declare const e: string | boolean | undefined
declare const u1: U1
declare let u2: U2
u1.email = e // error, but only because boolean isn't in email's type
//~^ ERROR: Type 'undefined | false | true | string' is not assignable to type 'undefined | number | string'.
u2.email = e // error, and suggest adding undefined
//~^ ERROR: Type 'undefined | false | true | string' is not assignable to type 'number | string'.
u2 = {
//~^ ERROR: Type '{ name: string; email: undefined; }' is not assignable to type 'U2'.
    name: 'hi',
    email: undefined
}

// Repro from #44437

declare var a: {[x: string]: number | string }
declare var b: {a: number, b: string}
declare var c: {a: number, b?: string}
declare var d: {a: number, b: string | undefined }
declare var e: {a: number, b?: string | undefined }
//~^ ERROR: Cannot redeclare block-scoped variable 'e'.

a = b;
a = c;
a = d;  // Error
//~^ ERROR: Type '{ a: number; b: undefined | string; }' is not assignable to type '{ [x: string]: number | string }'.
a = e;  // Error
//~^ ERROR: Type 'undefined | false | true | string' is not assignable to type '{ [x: string]: number | string }'.

// Repro from #46004

interface PropsFromReact {
    onClick?: () => void;
}

interface PropsFromMaterialUI {
    onClick?: (() => void) | undefined;
}

type TheTypeFromMaterialUI = PropsFromReact & PropsFromMaterialUI;

interface NavBottomListItem extends TheTypeFromMaterialUI {
    value: string;
}

// Repro from #46004

type UA = undefined;  // Explicit undefined type
type UB = { x?: never }['x'];  // undefined from missing property

type UC = UA & UB;  // undefined
