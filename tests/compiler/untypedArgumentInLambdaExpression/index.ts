// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/untypedArgumentInLambdaExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare function f(fn: (a: string) => string): string;
 
f((input): string => {
    return "." + input;
});

f((input): string => {
    let a: boolean = input;
    //~^ ERROR: Type 'string' is not assignable to type 'boolean'.
    return input
});
