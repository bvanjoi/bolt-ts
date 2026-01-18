// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/commaOperatorInConditionalExpression.ts`, Apache-2.0 License

function f (m: string) {
    [1, 2, 3].map(i => {
        return true? { [m]: i } : { [m]: i + 1 }
    })

    const a0: Record<string, number>[] = [1, 2, 3].map(i => {
        return true? { [m]: i } : { [m]: i + 1 }
    })
    const a1: {[m]: number} = {[m]: 42};
    const a2: {[m]: number} = {[m]: '42'};
    //~^ ERROR: Type 'string' is not assignable to type 'number'.
}