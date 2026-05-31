// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inKeywordAndUnknown.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

function f(x: {}, y: unknown) {
    if (!("a" in x)) {
        return;
    }
    x;  // {}
    if (!y) {
        return;
    }
    y;  // {}
    if (!("a" in y)) {
        //~^ ERROR: Type '{ }' may represent a primitive value, which is not permitted as the right operand of the 'in' operator.
        return;
    }
    y;  // {}
}

// Repro from #51007

function isHTMLTable(table: unknown): boolean {
    return !!table && table instanceof Object && 'html' in table;
}

function f1(x: unknown) {
    return x && x instanceof Object && 'a' in x;
}

function f2<T>(x: T) {
    return x && x instanceof Object && 'a' in x;
}

function f3(x: {}) {
    return x instanceof Object && 'a' in x;
}

function f4<T extends {}>(x: T) {
    return x instanceof Object && 'a' in x;
}

function f5<T>(x: T & {}) {
    return x instanceof Object && 'a' in x;
}

function f6<T extends {}>(x: T & {}) {
    return x instanceof Object && 'a' in x;
}

function f7<T extends object>(x: T & {}) {
    return x instanceof Object && 'a' in x;
}
