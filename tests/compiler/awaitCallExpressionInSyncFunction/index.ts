// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/awaitCallExpressionInSyncFunction.ts`, Apache-2.0 License

//@compiler-options: target=esnext

function foo() {
   const foo = await(Promise.resolve(1));
   //~^ ERROR: Cannot find name 'await'.
   return foo;
}