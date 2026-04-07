// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/discriminantsAndTypePredicates.ts`, Apache-2.0 License

interface A { type: 'A' }
interface B { type: 'B' }

function isA(x: A | B): x is A { return x.type === 'A'; }
function isB(x: A | B): x is B { return x.type === 'B'; }

function foo1(x: A | B): any {
    x;  // A | B
    let a: string = x;
    //~^ ERROR: Type 'A | B' is not assignable to type 'string'.
    if (isA(x)) {
        return x;  // A
    }
    x;  // B
    let b: string = x;
    //~^ ERROR: Type 'B' is not assignable to type 'string'.
    if (isB(x)) {
        return x;  // B
    }
    x;  // never

    x()
    //~^ ERROR: This expression is not callable.
}

function foo2(x: A | B): any {
    x;  // A | B
    let a: string = x;
    //~^ ERROR: Type 'A | B' is not assignable to type 'string'.
    if (x.type === 'A') {
        return x;  // A
    }
    x;  // B
    let b: string = x;
    //~^ ERROR: Type 'B' is not assignable to type 'string'.
    if (x.type === 'B') {
        return x;  // B
    }
    x;  // never

    x()
    //~^ ERROR: This expression is not callable.
}