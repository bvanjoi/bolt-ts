// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/asyncAwaitWithCapturedBlockScopeVar.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: lib=[es6]
//@compiler-options: noEmitHelpers

async function fn1() {
    let ar = [];
    for (let i = 0; i < 1; i++) {
        await 1;
        ar.push(() => i);
    }
}

async function fn2() {
    let ar = [];
    for (let i = 0; i < 1; i++) {
        await 1;
        ar.push(() => i);
        break;
    }
}

async function fn3() {
    let ar = [];
    for (let i = 0; i < 1; i++) {
        await 1;
        ar.push(() => i);
        continue;
    }
}

async function fn4(): Promise<number> {
  ///~^ ERROR: Function lacks ending return statement and return type does not include 'undefined'.
    let ar = [];
    for (let i = 0; i < 1; i++) {
        await 1;
        ar.push(() => i);
        return 1;
    }
}
