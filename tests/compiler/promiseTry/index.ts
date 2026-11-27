// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/promiseTry.ts`, Apache-2.0 License

//@compiler-options: target=esnext

Promise.try(() => {
    return "Sync result";
});

Promise.try(async () => {
    return "Async result";
});

const a = Promise.try(() => "Sync result");
const b = Promise.try(async () => "Async result");

// SINGLE PARAMETER
Promise.try((foo: string) => "Async result", "foo");
Promise.try((foo) => "Async result", "foo");
// @ts-expect-error too few parameters
Promise.try((foo) => "Async result");
Promise.try((foo: string | undefined) => "Async result", undefined);
Promise.try((foo: string | undefined) => "Async result", "foo");
Promise.try((foo) => "Async result", undefined);
// @ts-expect-error too many parameters
Promise.try(() => "Async result", "foo");

// MULTIPLE PARAMETERS
Promise.try((foo: string, bar: number) => "Async result", "foo", 42);
// @ts-expect-error too many parameters
Promise.try((foo: string, bar: number) => "Async result", "foo", 42, "baz");
// @ts-expect-error too few parameters
Promise.try((foo: string, bar: number) => "Async result", "foo");
Promise.try((foo: string, bar?: number) => "Async result", "foo");
Promise.try((foo: string, bar?: number) => "Async result", "foo", undefined);
Promise.try((foo: string, bar?: number) => "Async result", "foo", 42);
