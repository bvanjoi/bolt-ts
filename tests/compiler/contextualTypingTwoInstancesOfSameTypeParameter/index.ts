// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/contextualTypingTwoInstancesOfSameTypeParameter.ts`, Apache-2.0 License

function f6<T>(x: (a: T) => T) {
    return null;
} 
f6(x => f6(y => x = y));