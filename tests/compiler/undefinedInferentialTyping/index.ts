// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/undefinedInferentialTyping.ts`, Apache-2.0 License

function f<T>(arr: T[], elemnt: T): T {
    return null;
}

var a = f([], 3); // should be number