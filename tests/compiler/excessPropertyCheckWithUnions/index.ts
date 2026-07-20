// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/excessPropertyCheckWithUnions.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type ADT = {
    tag: "A",
    a1: string
} | {
    tag: "D",
    d20: 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20
} | {
    tag: "T",
}
let wrong: ADT = { tag: "T", a1: "extra" }  //~ ERROR: Object literal may only specify known properties, and 'a1' does not exist in type '{ tag: "T"; }'.
wrong = { tag: "A", d20: 12 }               //~ ERROR: Object literal may only specify known properties, and 'd20' does not exist in type '{ tag: "A"; a1: string; }'.
wrong = { tag: "D" }                        //~ ERROR: Type '{ tag: "D"; }' is not assignable to type 'ADT'.

type Ambiguous = {
    tag: "A",
    x: string
} | {
    tag: "A",
    y: number
} | {
    tag: "B",
    z: boolean
} | {
    tag: "C"
}
let amb: Ambiguous
// no error for ambiguous tag, even when it could satisfy both constituents at once
amb = { tag: "A", x: "hi" }
amb = { tag: "A", y: 12 }
amb = { tag: "A", x: "hi", y: 12 }

// correctly error on excess property 'extra', even when ambiguous
amb = { tag: "A", x: "hi", extra: 12 }  //~ERROR: Object literal may only specify known properties, and 'extra' does not exist in type '{ tag: "A"; x: string; } | { tag: "A"; y: number; }'.
amb = { tag: "A", y: 12, extra: 12 }    //~ERROR: Object literal may only specify known properties, and 'extra' does not exist in type '{ tag: "A"; x: string; } | { tag: "A"; y: number; }'.

// assignability errors still work
amb = { tag: "A" }                      //~ERROR: Type '{ tag: "A"; }' is not assignable to type 'Ambiguous'. 
amb = { tag: "A", z: true }             //~ERROR: Object literal may only specify known properties, and 'z' does not exist in type '{ tag: "A"; x: string; } | { tag: "A"; y: number; }'.

type Overlapping =
    | { a: 1, b: 1, first: string }
    | { a: 2, second: string }
    | { b: 3, third: string }
let over: Overlapping

// these two are still errors despite their doubled up discriminants
over = { a: 1, b: 1, first: "ok", second: "error" }   //~ERROR: Object literal may only specify known properties, and 'second' does not exist in type '{ a: 1; b: 1; first: string; }'.
over = { a: 1, b: 1, first: "ok", third: "error" }    //~ERROR: Object literal may only specify known properties, and 'third' does not exist in type '{ a: 1; b: 1; first: string; }'.

// Freshness disappears after spreading a union
declare let t0: { a: any, b: any } | { d: any, e: any }
declare let t1: { a: any, b: any, c: any } | { c: any, d: any, e: any }
let t2 = { ...t1 }
t0 = t2

// Nested excess property checks work with discriminated unions
type AN = { a: string } | { c: string }
type BN = { b: string }
type AB = { kind: "A", n: AN } | { kind: "B", n: BN }
const abab: AB = {
    kind: "A",
    n: {
        a: "a",
        b: "b", // excess -- kind: "A"
        //~^ ERROR: Object literal may only specify known properties, and 'b' does not exist in type 'AN'.
    }
}
const abac: AB = {
    kind: "A",
    n: {
        a: "a",
        c: "c", // ok -- kind: "A", an: { a: string } | { c: string }
    }
}

// Excess property checks must match all discriminable properties
type Button = { tag: 'button'; type?: 'submit'; };
type Anchor = { tag: 'a'; type?: string; href: string };

type Union = Button | Anchor;
const obj: Union = {
    tag: 'button',
    type: 'submit',

    // should have error here
    href: 'foo',              //~ERROR: Object literal may only specify known properties, and 'href' does not exist in type 'Button'.
};

// Repro from #34611

interface IValue {
  value: string
}

interface StringKeys {
    [propertyName: string]: IValue;
};

interface NumberKeys {
    [propertyName: number]: IValue;
}

type ObjectDataSpecification = StringKeys | NumberKeys;


const dataSpecification: ObjectDataSpecification = {  // Error
    foo: "asdfsadffsd"
    //~^ ERROR: Type 'string' is not assignable to type 'IValue'. 
};

// Repro from #34611

const obj1: { [x: string]: number } | { [x: number]: number } = { a: 'abc' };  // Error
  //~^ ERROR: Type 'string' is not assignable to type 'number'. 
const obj2: { [x: string]: number } | { a: number } = { a: 5, c: 'abc' };  // Error
  //~^ ERROR: Type 'string' is not assignable to type 'number'. 

// Repro from #33732

interface I1 {
    prop1: string;
}

interface I2 {
    prop2: string;
}

interface I3 extends Record<string, string> {

}

type Properties =
    | { [key: string]: never }
    | I1
    | I2
    | I3
    ;


declare const prop1: string;
declare const prop2: string | undefined;

function F1(_arg: { props: Properties }) { }
F1({
    props: {
        prop1,
        prop2,
    },
});

function F2(_props: Properties) { }
F2({
    prop1,
    prop2,
});
