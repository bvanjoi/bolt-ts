// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/contextualTypingFunctionReturningFunction.ts`, Apache-2.0 License

//@ run-fail

interface I {
	a(s: string): void;
	b(): (n: number) => void;
}

declare function f(i: I): void;

f({
	a: s => {},
	b: () => n => {},
});
