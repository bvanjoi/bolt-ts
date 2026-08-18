// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/truthinessPromiseCoercion.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: strictNullChecks

declare const p: Promise<number>
declare const p2: null | Promise<number>
declare const obj: { p: Promise<unknown> }
declare function pf(): Promise<boolean>

async function f() {
    if (p) {} // err
    //~^ ERROR: This condition will always return true since this 'Promise<number>' is always defined.
    if (!!p) {} // no err
    if (p2) {} // no err

    p ? f.arguments : f.arguments;
    //~^ ERROR: This condition will always return true since this 'Promise<number>' is always defined.
    !!p ? f.arguments : f.arguments;
    p2 ? f.arguments : f.arguments;
}

// all ok
async function g() {
    if (p) {
        p;
    }
    if (p && p.then.length) {}
    if (p) {
        if (p) {
            if (p) {
                !!await (((((((p)))))));
            }
        }
    }
}

async function h() {
    if (obj.p) {} // error
    //~^ ERROR: This condition will always return true since this 'Promise<unknown>' is always defined.
    if (obj.p) {  // ok
        await obj.p;
    }
    if (obj.p && await obj.p) {} // ok
}

async function i(): Promise<string> {
    if (pf()) { // error
    //~^ ERROR: This condition will always return true since this 'Promise<boolean>' is always defined.
        return "true";
    }
    if (pf()) { // error
    //~^ ERROR: This condition will always return true since this 'Promise<boolean>' is always defined.
        pf().then();
    }
    return "false";
}
