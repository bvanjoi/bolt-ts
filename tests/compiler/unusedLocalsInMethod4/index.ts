// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedLocalsInMethod4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

function f<T, NonNull extends {}>() {
    let x1: number[]; // should error
    let x2: number[] | null; // should error
    let x3: number[] | undefined; // should not error
    let x4: number[] | undefined | null; // should not error
    let x5!: number[]; // should not error
    let x6: any; // should not error
    let x7: unknown; // should not error
    let x8: T; // should error
    let x9: NonNull; // should error
    var x10: NonNull; // should not error
    let x11: NonNull; // should not error

    function foo() {
        console.log(x1);
        //~^ ERROR: Variable 'x1' is used before being assigned.
        console.log(x2);
        //~^ ERROR: Variable 'x2' is used before being assigned.
        console.log(x3);
        console.log(x4);
        console.log(x5);
        console.log(x6);
        console.log(x7);
        console.log(x8);
        //~^ ERROR: Variable 'x8' is used before being assigned.
        console.log(x9);
        //~^ ERROR: Variable 'x9' is used before being assigned.
        console.log(x10);
        console.log(x11);
    }
    function bar() {
        x11 = {} as any;
    }
    foo();
}

function f2<T, NonNull extends {}>() {
    let x1: number[]; // should error
    let x2: number[] | null; // should error
    let x3: number[] | undefined; // should not error
    let x4: number[] | undefined | null; // should not error
    let x5!: number[]; // should not error
    let x6: any; // should not error
    let x7: unknown; // should not error
    let x8: T; // should error
    let x9: NonNull; // should error

    console.log(x1);
    //~^ ERROR: Variable 'x1' is used before being assigned.
    console.log(x2);
    //~^ ERROR: Variable 'x2' is used before being assigned.
    console.log(x3);
    console.log(x4);
    console.log(x5);
    console.log(x6);
    console.log(x7);
    console.log(x8);
    //~^ ERROR: Variable 'x8' is used before being assigned.
    console.log(x9);
    //~^ ERROR: Variable 'x9' is used before being assigned.
}

function f3() {
    let x: number[];    // should error
    function foo() {
        x.toString();
        //~^ ERROR: Variable 'x' is used before being assigned.
    }
    foo();
}

function f4() {
    let x: number;  // should error
    return {
        foo() {
            return x.toString();
            //~^ ERROR: Variable 'x' is used before being assigned.
        }
    };
}

declare let x: number;  // should not error
function f5() {
    x.toString();
}
export default {};

function f6() {
    let key: string;    // should not error
    for (key in {}) {
        console.log(key);
    }
}

function f7() {
    let key: string;    // should not error
    for (key of []) {
        console.log(key);
    }
}

function f8() {
    function ff() {
        let _;
        let rest: {}; // should not error

        [_, ...rest] = bar();
    }
}
declare function bar(): [number, ...string[]];

function f9() {  
    const x: number; // should have only one error
    //~^ ERROR: Declarations must be initialized.
    function bar() {  
        let y = x;  
    }  
}  


function rw() {
    let i: number;  // should error
    function inside() {
        i++;
        //~^ ERROR: Variable 'i' is used before being assigned.
        console.log(i); // NaN
        //~^ ERROR: Variable 'i' is used before being assigned.
    }
    inside();
}
rw();

function createBinder() {
    var file: string;   // should not error

    function bindSourceFile(f: string) {
        file = f;

        file.toString();
    }
}

function transformClassFields() {
    enum ClassPropertySubstitutionFlags {
        ClassAliases = 1 << 0,
        ClassStaticThisOrSuperReference = 1 << 1,
    }

    let enabledSubstitutions: ClassPropertySubstitutionFlags;   // should error

    function enableSubstitutionForClassAliases() {
        enabledSubstitutions |= ClassPropertySubstitutionFlags.ClassAliases;
        //~^ ERROR: Variable 'enabledSubstitutions' is used before being assigned.

        enabledSubstitutions.toString();
        //~^ ERROR: Variable 'enabledSubstitutions' is used before being assigned.
    }
}