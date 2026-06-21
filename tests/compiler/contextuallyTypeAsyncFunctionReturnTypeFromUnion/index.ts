// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextuallyTypeAsyncFunctionReturnTypeFromUnion.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: noEmit
//@compiler-options: strict

// repro #47682

function f(a: () => Promise<{ count: number }>) {}
f((): Promise<{ count: number }> => Promise.reject("some err"));
f(async (): Promise<{ count: number }> => Promise.reject("some err"));

function g(a: () => Promise<{ count: number }> | StateMachine<{ count: number }>) {}
g(() => Promise.reject("some err"));
g(async () => Promise.reject("some err"));

declare class StateMachine<T> {
  onDone: (a: T) => void;
}

declare function createMachine<T>(implementations: {
  services: Record<string, () => Promise<T> | StateMachine<T>>;
}): void;

createMachine<{ count: number }>({
  services: {
    test: async () => Promise.reject("some err"),
    async test2() {
      return Promise.reject("some err");
    },
  },
});

function fn1(): () => Promise<{ count: number }> | StateMachine<{ count: number }> {
  return async () => Promise.reject('some err')
}

// repro #47682 issuecomment-1174099713

declare function load(): Promise<boolean>;

type LoadCallback = () => Promise<boolean> | string;

// all of those are essentially the same and should be allowed
const cb1: LoadCallback = async () => load().then(m => m);
const cb2: LoadCallback = async () => load();
const cb3: LoadCallback = () => load().then(m => m);