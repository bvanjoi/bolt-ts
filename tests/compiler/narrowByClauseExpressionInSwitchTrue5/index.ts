// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowByClauseExpressionInSwitchTrue5.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type A = { kind: "A", value: number };
type B = { kind: "B", name: string };
type C = { kind: "C", cond: boolean };
type D = { kind: "D", value: boolean };
type E = { kind: "E", x: number, y: number };

type All = A | B | C | D | E;

function fn1switch(input: All) {
    switch (true) {
        case input.kind === "A":
        case input.kind === "B":
            if (input.kind === "A") {
                return;
            }
            const a: 'A' = input.kind;
            //~^ ERROR: Type '"B"' is not assignable to type '"A"'.
            const a0: 'B' = input.kind;
            input; // Should be B;
            //  ^?

            // fallthrough
        case input.kind === "C":
            input; // Should be B | C
            //  ^?
            const b: 'A' = input.kind;
            //~^ ERROR: Type 'string' is not assignable to type '"A"'.
            const b0: 'B' | 'C' = input.kind;
            break;
        default:
            input; // Should be D | E
            //  ^?
            const c: 'A' = input.kind;
            //~^ ERROR: Type 'string' is not assignable to type '"A"'.
            const c0: 'D' | 'E' = input.kind;
    }

    input; // Should not be A
    //  ^?
    const d: 'A' = input.kind;
    //~^ ERROR: Type 'string' is not assignable to type '"A"'.
    const d0: 'B' | 'C' | 'D' | 'E' = input.kind;
    const d1: 'B' = input.kind;
    //~^ ERROR: Type 'string' is not assignable to type '"B"'.
}

function fn1ifelse(input: All) {
    if (input.kind === "A" || input.kind === "B") {
        if (input.kind === "A") {
            return;
        }

        input; // Should be B;
        //  ^?
        const a: 'A' = input.kind;
        //~^ ERROR: Type '"B"' is not assignable to type '"A"'.
        const a0: 'B' = input.kind;
    }
    if (input.kind === "C" || input.kind === "B") {
        input; // Should be B | C
        //  ^?
        const b: 'A' = input.kind;
        //~^ ERROR: Type 'string' is not assignable to type '"A"'.
        const b0: 'B' | 'C' = input.kind;
    }
    else {
        input; // Should be D | E
        //  ^?
        const c: 'A' = input.kind;
        //~^ ERROR: Type 'string' is not assignable to type '"A"'.
        const c0: 'D' | 'E' = input.kind;
    }

    input; // Should not be A
    //  ^?
    const d: 'A' = input.kind;
    //~^ ERROR: Type 'string' is not assignable to type '"A"'.
    const d0: 'B' | 'C' | 'D' | 'E' = input.kind;
    const d1: 'B' = input.kind;
    //~^ ERROR: Type 'string' is not assignable to type '"B"'.
}

function fn2switch(input: All) {
    switch (true) {
        case input.kind === "A":
        case input.kind === "B":
            if (input.kind === "A") {
                return;
            }

            input; // Should be B;
            //  ^?
            const a: 'A' = input.kind;
            //~^ ERROR: Type '"B"' is not assignable to type '"A"'.
            const a0: 'B' = input.kind;

            // fallthrough
        case input.kind === "C":
            input; // Should be B | C
            //  ^?
            const b: 'A' = input.kind;
            //~^ ERROR: Type 'string' is not assignable to type '"A"'.
            const b0: 'B' | 'C' = input.kind;
            break;
        default:
            input; // Should be D | E
            //  ^?
            const c: 'A' = input.kind;
            //~^ ERROR: Type 'string' is not assignable to type '"A"'.
            const c0: 'D' | 'E' = input.kind;
            return;
    }

    input; // Should be B | C
    //  ^?
   const d: 'A' = input.kind;
    //~^ ERROR: Type 'string' is not assignable to type '"A"'.
    const d0: 'B' | 'C' = input.kind;
    const d1: 'B' = input.kind;
    //~^ ERROR: Type 'string' is not assignable to type '"B"'.
}

function fn2ifelse(input: All) {
    if (input.kind === "A" || input.kind === "B") {
        if (input.kind === "A") {
            return;
        }

        input; // Should be B;
        //  ^?
        const a: 'A' = input.kind;
        //~^ ERROR: Type '"B"' is not assignable to type '"A"'.
        const a0: 'B' = input.kind;
    }
    if (input.kind === "C" || input.kind === "B") {
        input; // Should be B | C
        //  ^?
        const b: 'A' = input.kind;
        //~^ ERROR: Type 'string' is not assignable to type '"A"'.
        const b0: 'B' | 'C' = input.kind;
    }
    else {
        input; // Should be D | E
        //  ^?
        const c: 'A' = input.kind;
        //~^ ERROR: Type 'string' is not assignable to type '"A"'.
        const c0: 'D' | 'E' = input.kind;
        return;
    }

    input; // Should be B | C
    //  ^?
    const d: 'A' = input.kind;
    //~^ ERROR: Type 'string' is not assignable to type '"A"'.
    const d0: 'B' | 'C' = input.kind;
    const d1: 'B' = input.kind;
    //~^ ERROR: Type 'string' is not assignable to type '"B"'.
}

function fn3switch(input: All) {
    switch (true) {
        case input.kind === "A":
        case input.kind === "B":
            if (input.kind === "A") {
                return;
            }

            input; // Should be B;
            //  ^?
            const a: 'A' = input.kind;
            //~^ ERROR: Type '"B"' is not assignable to type '"A"'.
            const a0: 'B' = input.kind;

            // fallthrough
        default:
            input; // Should be B | D | E
            //  ^?
            const b: 'A' = input.kind;
            //~^ ERROR: Type 'string' is not assignable to type '"A"'.
            const b0: 'B' | 'D' | 'E' = input.kind;
            break;

        case input.kind === "C":
            input; // Should be C
            //  ^?
            const c: 'A' = input.kind;
            //~^ ERROR: Type '"C"' is not assignable to type '"A"'.
            const c0: 'C' = input.kind;
            break;
    }

    input; // Should not be A
    //  ^?
    const d: 'A' = input.kind;
    //~^ ERROR: Type 'string' is not assignable to type '"A"'.
    const d0: 'B' | 'C' | 'D' | 'E' = input.kind;
    const d1: 'B' = input.kind;
    //~^ ERROR: Type 'string' is not assignable to type '"B"'.
}
