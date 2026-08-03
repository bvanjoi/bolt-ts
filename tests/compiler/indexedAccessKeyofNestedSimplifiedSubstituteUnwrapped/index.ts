// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexedAccessKeyofNestedSimplifiedSubstituteUnwrapped.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type AnyFunction = (...args: any[]) => any;
type Params<T> = Parameters<Extract<T, AnyFunction>>;

interface Wrapper<T> {
	call<K extends keyof T>(event: K, ...args: Params<T[K]>): void;
}

interface AWrapped {
	foo(): void;
}

class A {
	foo: Wrapper<AWrapped>;
  //~^ ERROR: Property 'foo' has no initializer and is not definitely assigned in the constructor.
}

interface BWrapped extends AWrapped {
	bar(): void;
}

class B extends A {
	foo: Wrapper<BWrapped>;
  //~^ ERROR: Property 'foo' has no initializer and is not definitely assigned in the constructor.
}