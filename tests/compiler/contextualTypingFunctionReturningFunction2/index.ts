// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/contextualTypingFunctionReturningFunction2.ts`, Apache-2.0 License

//@ run-fail

declare function f(n: number): void;
declare function f(cb: () => (n: number) => number): void;

f(() => n => n);
