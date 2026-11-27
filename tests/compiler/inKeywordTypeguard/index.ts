// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inKeywordNarrowingWithNoUncheckedIndexedAccess.ts`, Apache-2.0 License

//@[strict=true]   compiler-options: strict
//@[strict=false]  compiler-options: strict=false
//@compiler-options: target=es2015

class A { a: string; }
//~[strict=true]^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
class B { b: string; }
//~[strict=true]^ ERROR: Property 'b' has no initializer and is not definitely assigned in the constructor.

function negativeClassesTest(x: A | B) {
    if ("a" in x) {
        x.b = "1";
        //~^ ERROR: Property 'b' does not exist on type 'A'.
    } else {
        x.a = "1";
        //~^ ERROR: Property 'a' does not exist on type 'B'.
    }
}

function positiveClassesTest(x: A | B) {
    if ("a" in x) {
        x.b = "1";
        //~^ ERROR: Property 'b' does not exist on type 'A'.
    } else {
        x.a = "1";
        //~^ ERROR: Property 'a' does not exist on type 'B'.
    }
}

class AWithOptionalProp { a?: string; }
class BWithOptionalProp { b?: string; }

function positiveTestClassesWithOptionalProperties(x: AWithOptionalProp | BWithOptionalProp) {
    if ("a" in x) {
        x.a = "1";
    } else {
        x.b = "1";
        //~^ ERROR: Property 'b' does not exist on type 'AWithOptionalProp | BWithOptionalProp'.
    }
}

class AWithMethod {
    a(): string { return ""; }
}

class BWithMethod {
    b(): string { return ""; }
}

function negativeTestClassesWithMembers(x: AWithMethod | BWithMethod) {
    if ("a" in x) {
        x.a();
        x.b();
        //~^ ERROR: Property 'b' does not exist on type 'AWithMethod'.
    } else {
    }
}

function negativeTestClassesWithMemberMissingInBothClasses(x: AWithMethod | BWithMethod) {
    if ("c" in x) {
        x.a();
        //~^ ERROR: Property 'a' does not exist on type 'AWithMethod & Record<"c", unknown> | BWithMethod & Record<"c", unknown>'.
        x.b();
        //~^ ERROR: Property 'b' does not exist on type 'AWithMethod & Record<"c", unknown> | BWithMethod & Record<"c", unknown>'.
    } else {
        x.a();
        //~^ ERROR: Property 'a' does not exist on type 'AWithMethod | BWithMethod'.
        x.b();
        //~^ ERROR: Property 'b' does not exist on type 'AWithMethod | BWithMethod'.
    }
}

class C { a: string; }
//~[strict=true]^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
class D { a: string; }
//~[strict=true]^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.

function negativeMultipleClassesTest(x: A | B | C | D) {
    if ("a" in x) {
        x.b = "1";
        //~^ ERROR: Property 'b' does not exist on type 'A | C | D'.
    } else {
        x.a = "1";
        //~^ ERROR: Property 'a' does not exist on type 'B'.
    }
}

class ClassWithUnionProp { prop: A | B }
//~[strict=true]^ ERROR: Property 'prop' has no initializer and is not definitely assigned in the constructor.

function negativePropTest(x: ClassWithUnionProp) {
    if ("a" in x.prop) {
        let y: string = x.prop.b;
        //~^ ERROR: Property 'b' does not exist on type 'A'.
    } else {
        let z: string = x.prop.a;
        //~^ ERROR: Property 'a' does not exist on type 'B'.
    }
}

class NegativeClassTest {
    protected prop: A | B;
    //~[strict=true]^ ERROR: Property 'prop' has no initializer and is not definitely assigned in the constructor.
    inThis() {
        if ("a" in this.prop) {
            let z: number = this.prop.b;
            //~^ ERROR: Property 'b' does not exist on type 'A'.
        } else {
            let y: string = this.prop.a;
            //~^ ERROR: Property 'a' does not exist on type 'B'.
        }
    }
}

class UnreachableCodeDetection {
    a: string;
    //~[strict=true]^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
    inThis() {
        if ("a" in this) {
        } else {
            let y = this.a;
            //~^ ERROR: Property 'a' does not exist on type 'never'.
        }
    }
}

function positiveIntersectionTest(x: { a: string } & { b: string }) {
    if ("a" in x) {
        let s: string = x.a;
    } else {
        let n: never = x;
    }
}

// Repro from #38608
declare const error: Error;
if ('extra' in error) {
    error // Still Error
} else {
    error // Error
}

function narrowsToNever(x: { l: number } | { r: number }) {
    let v: number;
    if ("l" in x) {
        v = x.l;
    }
    else if ("r" in x) {
        v = x.r;
    }
    else {
        v = x
    }
    return v;
}

type AOrB = { aProp: number } | { bProp: number };
declare function isAOrB(x: unknown): x is AOrB;

declare var x: unknown;
if (isAOrB(x)) {
    if ("aProp" in x) {
        x.aProp;
    }
    else if ("bProp" in x) {
        x.bProp;
    }
    // x is never because of the type predicate from unknown
    else if ("cProp" in x) {
        const _never: never = x;
    }
}

function negativeIntersectionTest() {
    if ("ontouchstart" in window) {
        window.ontouchstart
    } else {
        window.ontouchstart
    }
}

function f1(x: unknown) {
    if ("a" in x) {
        //~[strict=true]^ ERROR: Object is of type 'unknown'.
        //~[strict=false]^^ ERROR: The right value of the `in` operator must be an 'object', but got 'unknown'.
        x.a;
    }
    if (x && "a" in x) {
        //~[strict=true]^ ERROR: Type '{ }' may represent a primitive value, which is not permitted as the right operand of the 'in' operator.
        //~[strict=true]| ERROR: Type '{ }' may represent a primitive value, which is not permitted as the right operand of the 'in' operator.
        //~[strict=false]^^^ ERROR: The right value of the `in` operator must be an 'object', but got 'unknown'.
        x.a;
    }
    if (x && typeof x === "object" && "a" in x) {
        x.a;
    }
    if (x && typeof x === "object" && "a" in x && "b" in x && "c" in x) {
        x.a;
        x.b;
        x.c;
    }
}

function f2(x: object) {
    if ("a" in x) {
        x.a;
    }
    if ("a" in x && "b" in x && "c" in x) {
        x.a;
        x.b;
        x.c;
    }
}

function f3<T>(x: T) {
    if ("a" in x) {
        //~[strict=true]^ ERROR: right value of the `in` operator must be an 'object', but got 'T'.
        //~[strict=false]^^ ERROR: The right value of the `in` operator must be an 'object', but got 'T'.
        x.a;
    }
    if (x && "a" in x) {
        //~[strict=true]^ ERROR: Type 'NonNullable<T>' may represent a primitive value, which is not permitted as the right operand of the 'in' operator.
        //~[strict=true]| ERROR: Type 'NonNullable<T>' may represent a primitive value, which is not permitted as the right operand of the 'in' operator.
        //~[strict=false]^^^ ERROR: The right value of the `in` operator must be an 'object', but got 'T'.
        x.a;
    }
    if (x && typeof x === "object" && "a" in x) {
        x.a;
    }
    if (x && typeof x === "object" && "a" in x && "b" in x && "c" in x) {
        x.a;
        x.b;
        x.c;
    }
}

function f4(x: { a: string }) {
    if ("a" in x) {
        x.a;
    }
    if ("a" in x && "b" in x && "c" in x) {
        x.a;
        x.b;
        x.c;
    }
}

function f5(x: { a: string } | { b: string }) {
    if ("a" in x) {
        x;  // { a: string }
    }
    else if ("b" in x) {
        x;  // { b: string }
    }
    else {
        x;  // never
    }
}

function f6(x: { a: string } | { b: string }) {
    if ("a" in x) {
        x;  // { a: string }
    }
    else if ("a" in x) {
        x;  // { b: string } & Record<"a", unknown>
    }
    else {
        x;  // { b: string }
    }
}

// Object and corresponding intersection should narrow the same

function f7(x: { a: string, b: number }, y: { a: string } & { b: number }) {
    if ("a" in x) {
        x;
    }
    else {
        x;  // never
    }
    if ("a" in y) {
        y;
    }
    else {
        y;  // never
    }
}

const sym = Symbol();

function f8(x: object) {
    if ("a" in x && 1 in x && sym in x) {
        x.a;
        x["a"];
        x[1];
        x["1"];
        x[sym];
    }
}

function f9(x: object) {
    if ("a" in x && "1" in x && sym in x) {
        x.a;
        x["a"];
        x[1];
        x["1"];
        x[sym];
    }
}

function f10(x: { a: unknown }) {
    if ("a" in x) {
        x;
    }
    else {
        x;
    }
}

function f11(x: { a: any }) {
    if ("a" in x) {
        x;
    }
    else {
        x;
    }
}

function f12(x: { a: string }) {
    if ("a" in x) {
        x;
    }
    else {
        x;
    }
}

function f13(x: { a?: string }) {
    if ("a" in x) {
        x;
    }
    else {
        x;
    }
}

function f14(x: { a: string | undefined }) {
    if ("a" in x) {
        x;
    }
    else {
        x;
    }
}

function f15(x: { a?: string | undefined }) {
    if ("a" in x) {
        x;
    }
    else {
        x;
    }
}

function f16(x: typeof globalThis, y: Window & typeof globalThis) {
    x = y;
}

// Repro from #50639

function foo<A>(value: A) {
    if (typeof value === "object" && value !== null && "prop" in value) {
        value;  // A & object & Record<"prop", unknown>
    }
}

// Repro from #50954

const checkIsTouchDevice = () =>
    "ontouchstart" in window || "msMaxTouchPoints" in window.navigator;

// Repro from #51501

function isHTMLTable<T extends object | null>(table: T): boolean {
    return !!table && 'html' in table;
}

// Repro from #51549

const f = <P extends object>(a: P & {}) => {
    "foo" in a;
};

// Repro from #53773

function test1<T extends any[] | Record<string, any>>(obj: T) {
    if (Array.isArray(obj) || 'length' in obj) {
      obj;  // T
    }
    else {
      obj;  // T
    }
}

function test2<T extends any[] | Record<string, any>>(obj: T) {
    if (Array.isArray(obj)) {
      obj;  // T & any[]
    }
    else {
      obj;  // T
    }
}

function test3<T extends any[] | Record<string, any>>(obj: T) {
    if ('length' in obj) {
      obj;  // T
    }
    else {
      obj;  // T
    }
}
