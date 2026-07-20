// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/spreadOfParamsFromGeneratorMakesRequiredParams.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare function call<Fn extends (...args: any[]) => any>(
    fn: Fn,
    ...args: Parameters<Fn>
): any;

call(function* (a: 'a') { }); // error, 2nd argument required
//~^ ERROR: Expected 2 arguments, but got 1.
