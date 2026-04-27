// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/indexedAccessConstraints.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo<T extends object>(a: T[keyof T]) {
    let b: number = a;  // Error
    //~^ ERROR: Type 'T[keyof T]' is not assignable to type 'number'.
}

// Repro from #54522

export function methodFnLength<T extends {}, K extends keyof T>(obj: T, methodKey: K): number {
    const fn = obj[methodKey];
    if (typeof fn !== 'function') {
        return 0;
    }
    return fn.length;
}

// Repro from #54837

function getField<T extends object>(x: T | null, k: keyof T) {
    const result = x ? x[k] : null;
    return result;  // T[keyof T] | null
}
