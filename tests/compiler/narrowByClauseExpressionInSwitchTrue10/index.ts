// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowByClauseExpressionInSwitchTrue10.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

function foo(cond1: boolean, cond2: boolean) {
    switch (true) {
        case cond1 || cond2:
            cond1; // boolean
            //  ^?
            cond2; // boolean
            //  ^?
            const a0: 42 = cond1;   //~ERROR: Type 'boolean' is not assignable to type '42'.
            const a1: 42 = cond2;   //~ERROR: Type 'boolean' is not assignable to type '42'.
            break;

        case cond2:
            cond1; // false
            //  ^?
            cond2;; // never
            //  ^?
            const a20: true = cond1;   //~ERROR: Type 'boolean' is not assignable to type 'true'.
            const a21: false = cond1;
            const a3: 42 = cond2;
            break;

        default:
            cond1; // false
            //  ^?
            cond2; // false
            //  ^?
            const a40: true = cond1;    //~ERROR: Type 'boolean' is not assignable to type 'true'.
            const a41: false = cond1;
            const a50: true = cond2;    //~ERROR: Type 'boolean' is not assignable to type 'true'.
            const a51: false = cond2;
            break;
    }

    cond1; // boolean
    //  ^?
    cond2; // boolean
    //  ^?
    const a6: 42 = cond1;   //~ERROR: Type 'boolean' is not assignable to type '42'.
    const a7: 42 = cond2;   //~ERROR: Type 'boolean' is not assignable to type '42'.
}

function blah(cond1: boolean, cond2: boolean) {
    if (cond1 || cond2) {
        cond1; // boolean
        //  ^?
        cond2; // boolean
        //  ^?
        const a0: 42 = cond1;   //~ERROR: Type 'boolean' is not assignable to type '42'.
        const a1: 42 = cond2;   //~ERROR: Type 'boolean' is not assignable to type '42'.
    } else if (cond2) {
        cond1; // false
        //  ^?
        cond2; // never
        //  ^?
        const a20: true = cond1;   //~ERROR: Type 'boolean' is not assignable to type 'true'.
        const a21: false = cond1;
        const a1: 42 = cond2;
    } else {
        cond1; // false
        //  ^?
        cond2; // false
        //  ^?
        const a40: true = cond1;    //~ERROR: Type 'boolean' is not assignable to type 'true'.
        const a41: false = cond1;
        const a50: true = cond2;    //~ERROR: Type 'boolean' is not assignable to type 'true'.
        const a51: false = cond2;
    }

    cond1; // boolean
    //  ^?
    cond2; // boolean
    //  ^?
    const a0: 42 = cond1;   //~ERROR: Type 'boolean' is not assignable to type '42'.
    const a1: 42 = cond2;   //~ERROR: Type 'boolean' is not assignable to type '42'.
}
