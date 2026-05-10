// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentToAnyArrayRestParameters.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

// Repros from #57122

function foo<T extends string[]>(
    fa: (s: string, ...args: string[]) => string,
    fb: (s: string, ...args: T) => string
) {
    const f1: (...args: any) => string = fa;
    const f2: (...args: any[]) => string = fa;
    const f3: (...args: any) => string = fb;
    const f4: (...args: any[]) => string = fb;
}

function bar<T extends string[], K extends number>() {
    type T00 = string[]["0"];
    type T01 = string[]["0.0"];  // Error
    //~^ ERROR: Property '"0.0"' does not exist on type 'string[]'.
    type T02 = string[][K | "0"];
    type T10 = T["0"];
    type T11 = T["0.0"];  // Error
    //~^ ERROR: Type '"0.0"' cannot be used to index type 'T'.
    type T12 = T[K | "0"];
}
