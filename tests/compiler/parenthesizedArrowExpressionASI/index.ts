// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parenthesizedArrowExpressionASI.ts`, Apache-2.0 License

//@compiler-options: target=es2015

const x = (a: any[]) => (
    // comment
    undefined as number
    //~^ ERROR: Conversion of type 'undefined' to type 'number' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
);