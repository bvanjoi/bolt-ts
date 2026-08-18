// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/simpleRecursionWithBaseCase1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noImplicitAny

function fn1(n: number) {
    if (n === 0) {
        return 3;
    } else {
        return fn1(n - 1);
    }
}
const num: number = fn1();
//~^ ERROR: Expected 1 arguments, but got 0.

function fn2(n: number) {
    return fn2(n);
}
const nev: never = fn2();
//~^ ERROR: Expected 1 arguments, but got 0.

function fn3(n: number) {
    if (n === 0) {
        return 3;
    } else {
        return fn1("hello world");
        //~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.
    }
}

function fn4(n: number) {
    if (n === 0) {
        return 3;
    } else {
        return notfoundsymbol("hello world");
        //~^ ERROR: Cannot find name 'notfoundsymbol'.
    }
}

function fn5() {
  //~^ ERROR: 'fn5' implicitly has return type 'any' because it does not have a return type annotation and is referenced directly or indirectly in one of its return expressions.
    return [fn5][0]();
}
