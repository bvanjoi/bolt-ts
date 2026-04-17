// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/typePredicatesInUnion3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type P1 = (x: unknown) => x is string;
type P2 = (x: unknown) => x is number;

type F1 = (x: unknown) => false;
type F2 = (x: unknown) => boolean;
type F3 = (x: unknown) => string;

function f1(x: unknown, p: P1 | P2) {
    if (p(x)) {
        x;  // string | number
        let a: 42 = x;  // string | number
        //~^ ERROR: Type 'number | string' is not assignable to type '42'.
    }
}

function f2(x: unknown, p: P1 | P2 | F1) {
    if (p(x)) {
        x;  // string | number
        let a: 42 = x;  // string | number
        //~^ ERROR: Type 'number | string' is not assignable to type '42'.
    }
}

function f3(x: unknown, p: P1 | P2 | F2) {
    if (p(x)) {
        x;  // unknown
    }
}

function f4(x: unknown, p: P1 | P2 | F3) {
    if (p(x)) {
        x;  // unknown
    }
}

// Repro from #54143

type HasAttribute<T> = T & { attribute: number };

class Type1 {
    attribute: number | null = null;
    predicate(): this is HasAttribute<Type1> {
        return true;
    }
}

class Type2 {
    attribute: number | null = null;
    predicate(): boolean {
        return true;
    }
}

function assertType<T>(_val: T) {
}

declare const val: Type1 | Type2;

if (val.predicate()) {
    assertType<number>(val.attribute);  // Error
    //~^ ERROR: Argument of type 'null | number' is not assignable to parameter of type 'number'.
}
