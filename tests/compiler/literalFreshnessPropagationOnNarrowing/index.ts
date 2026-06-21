// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/literalFreshnessPropagationOnNarrowing.ts`, Apache-2.0 License

//@compiler-options: target=es2015

const c = true;
let obj2 = { c };
obj2.c = false;

function f1() {
    let b = true;
    let obj = { b };
    // Desired: OK
    // 3.0: OK
    // 3.1 as-is: OK
    // 3.1 minus widening propagation: error
    obj.b = false;
}

function f2() {
    type Element = (string | false);
    type ElementOrArray = Element | Element[]; 
    let el: Element = null as any;
    let arr: Element[] = null as any;
    let elOrA: ElementOrArray = null as any;

    // Desired/actual: All OK
    let a1: ElementOrArray = el;
    let a2: ElementOrArray = arr;
    let a3: ElementOrArray = [el];
    let a4: ElementOrArray = Array.isArray(elOrA) ? elOrA : [elOrA];

    // Desired: OK
    // 3.0: Error
    // 3.1: OK
    let a5: ElementOrArray = [...Array.isArray(elOrA) ? elOrA : [elOrA]];
}

function f3() {
    type XY = 'x' | 'y';
    const x: XY = 'x';
    let x2 = x;
    // Desired: OK (up for debate?)
    // 3.0: Error
    // 3.1 as-is: OK
    x2 = 'y';
    //~^ ERROR: Type '"y"' is not assignable to type '"x"'.

    // Desired/actual: All OK
    let x3: XY = x;
    x3 = 'y';
}

function f4() {
    const x: boolean = true;
    let x1 = x;
    // Desired: OK
    // 3.0: OK
    // 3.1: OK
    // 3.1 minus widening propagation: error
    x1 = false;
}

function f5() {
    type XY = 'x' | 'y';
    let arr: XY[] = ['x'];
    arr = ['y'];
    // Desired: OK
    // Error in all extant branches
    arr = [...['y']];
    //~^ ERROR: Type 'string[]' is not assignable to type '(XY)[]'.
}