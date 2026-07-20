// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inKeywordNarrowingWithNoUncheckedIndexedAccess.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit
//@compiler-options: noUncheckedIndexedAccess

declare function invariant(condition: boolean): asserts condition;

function f1(obj: Record<string, string>) {
    invariant("test" in obj);
    let a: number = obj.test;  
    //~^ Error: Type 'string' is not assignable to type 'number'.
    let b: number = obj.test2;
    //~^ Error: Type 'undefined | string' is not assignable to type 'number'.
    return obj.test;  // string
}

function f2(obj: Record<string, string>) {
    if ("test" in obj) {
        let a: number = obj.test;  
        //~^ Error: Type 'string' is not assignable to type 'number'.
        return obj.test;  // string
    }
    return "default";
}

function f3(obj: Record<string, string>) {
    obj.test;  // string | undefined
    if ("test" in obj) {
        let a: number = obj.test;  
        //~^ Error: Type 'string' is not assignable to type 'number'.
        obj.test;  // string
    }
    else {
        let a: number = obj.test;  
        //~^ Error: Type 'undefined' is not assignable to type 'number'.
        obj.test;  // undefined
    }
}

function f4(obj: Record<string, string>) {
    obj.test;  // string | undefined
    if (obj.hasOwnProperty("test")) {
        obj.test;  // string
        let a: number = obj.test;  
        //~^ Error: Type 'string' is not assignable to type 'number'.
    }
    else {
        obj.test;  // undefined
        let a: number = obj.test;  
        //~^ Error: Type 'undefined' is not assignable to type 'number'.
    }
}
