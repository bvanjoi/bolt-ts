// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/promiseTry.ts`, Apache-2.0 License
//@compiler-options: target=esnext
Promise.try(() => ('Sync result'));
Promise.try(async () => ('Async result'));
var a = Promise.try(() => ('Sync result'));
var b = Promise.try(async () => ('Async result'));
Promise.try(// SINGLE PARAMETER
(foo) => ('Async result'), 'foo');
Promise.try((foo) => ('Async result'), 'foo');
// @ts-expect-error too few parameters
Promise.try((foo) => ('Async result'));
Promise.try((foo) => ('Async result'), undefined);
Promise.try((foo) => ('Async result'), 'foo');
Promise.try((foo) => ('Async result'), undefined);
// @ts-expect-error too many parameters
Promise.try(() => ('Async result'), 'foo');
// MULTIPLE PARAMETERS
Promise.try((foo, bar) => ('Async result'), 'foo', 42);
// @ts-expect-error too many parameters
Promise.try((foo, bar) => ('Async result'), 'foo', 42, 'baz');
// @ts-expect-error too few parameters
Promise.try((foo, bar) => ('Async result'), 'foo');
Promise.try((foo, bar) => ('Async result'), 'foo');
Promise.try((foo, bar) => ('Async result'), 'foo', undefined);
Promise.try((foo, bar) => ('Async result'), 'foo', 42);