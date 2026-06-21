// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowByClauseExpressionInSwitchTrue8.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

function foo(cond1: boolean, cond2: boolean) {
    switch (true) {
        case cond1:
            cond1; // Should be true
            //  ^?
            cond2; // Should be boolean
            //  ^?
            const a0: true = cond1;
            const a1: false = cond1;
            //~^ ERROR: Type 'boolean' is not assignable to type 'false'.
            const a2: true = cond2;
            //~^ ERROR: Type 'boolean' is not assignable to type 'true'.
            const a3: false = cond2;
            //~^ ERROR: Type 'boolean' is not assignable to type 'false'.
            break;

        case cond2:
            cond1; // Should be false?
            //  ^?
            cond2; // Should be true
            //  ^?
            const b0: true = cond1;
            //~^ ERROR: Type 'boolean' is not assignable to type 'true'.
            const b1: false = cond1;
            const b2: true = cond2;
            const b3: false = cond2;
            //~^ ERROR: Type 'boolean' is not assignable to type 'false'.
            break;

        default:
            cond1; // Should be false?
            //  ^?
            cond2; // Should be false?
            //  ^?
            const c0: true = cond1;
            //~^ ERROR: Type 'boolean' is not assignable to type 'true'.
            const c1: false = cond1;
            const c2: true = cond2;
            //~^ ERROR: Type 'boolean' is not assignable to type 'true'.
            const c3: false = cond2;
            break;
    }

    cond1; // Should be boolean
    //  ^?
    cond2; // Should be boolean
    //  ^?
    const d0: true = cond1;
    //~^ ERROR: Type 'boolean' is not assignable to type 'true'.
    const d1: false = cond1;
    //~^ ERROR: Type 'boolean' is not assignable to type 'false'.
    const d2: true = cond2;
    //~^ ERROR: Type 'boolean' is not assignable to type 'true'.
    const d3: false = cond2;
    //~^ ERROR: Type 'boolean' is not assignable to type 'false'.
}
