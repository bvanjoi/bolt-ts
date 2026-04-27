// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/circularInferredTypeOfVariable.ts`, Apache-2.0 License

//@compiler-options: target=es6

(async () => {
    function foo(p: string[]): string[] {
        return [];
    }

    function bar(p: string[]): string[] {
        return [];
    }

    let a1: string[] | undefined = [];

    while (true) {
        let a2 = foo(a1!);
        a1 = await bar(a2);
    }
});