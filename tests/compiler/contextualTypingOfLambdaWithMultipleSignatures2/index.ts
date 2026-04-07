// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/contextualTypingOfLambdaWithMultipleSignatures2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var f: {
    (x: string): string;
    (x: number): string
};

f = (a) => { return a.asdf }