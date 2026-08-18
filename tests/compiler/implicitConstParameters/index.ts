// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implicitConstParameters.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

function doSomething(cb: () => void) {
    cb();
}

function fn(x: number | string) {
  if (typeof x === 'number') {
      doSomething(() => x.toFixed());
  }
}

function f1(x: string | undefined) {
    if (!x) {
        return;
    }
    doSomething(() => x.length);
}

function f2(x: string | undefined) {
    if (x) {
        doSomething(() => {
            doSomething(() => x.length);
        });
    }
}

function f3(x: string | undefined) {
    inner();
    function inner() {
        if (x) {
            doSomething(() => x.length);
        }
    }
}

function f4(x: string | undefined) {
    x = "abc";
    if (x) {
        doSomething(() => x.length);
    }
}

function f5(x: string | undefined) {
    if (x) {
        doSomething(() => x.length);
        //~^ ERROR: 'x' is possibly 'undefined'.
        //~| ERROR: 'x' is possibly 'undefined'.
    }
    x = "abc";  // causes x to be considered non-const
}


function f6(x: string | undefined) {
    const y = x || "";
    if (x) {
        doSomething(() => y.length);
    }
}