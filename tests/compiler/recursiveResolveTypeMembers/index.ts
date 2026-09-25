// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveResolveTypeMembers.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type PromisedTuple<L extends any[], U = (...args: L) => void> =
    U extends (h: infer H, ...args: infer R) => [Promise<H>, ...PromisedTuple<R>] ? [] : []
    //~^ ERROR: Return type annotation circularly references itself.
    //~| ERROR: Return type annotation circularly references itself.
    //~| ERROR: Cannot find name 'H'.
    //~| ERROR: Cannot find name 'R'.

type Promised = PromisedTuple<[1, 2, 3]> 
