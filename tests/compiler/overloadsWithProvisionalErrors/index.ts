// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadsWithProvisionalErrors.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare var func: {
    (s: string): number;
    (lambda: (s: string) => { a: number; b: number }): string;
};

func(s => ({})); // Error for no applicable overload (object type is missing a and b)
//~^ ERROR: No overload matches this call.
func(s => ({ a: blah, b: 3 })); // Only error inside the function, but not outside (since it would be applicable if not for the provisional error)
//~^ ERROR: Cannot find name 'blah'.
func(s => ({ a: blah })); // Two errors here, one for blah not being defined, and one for the overload since it would not be applicable anyway
//~^ ERROR: Cannot find name 'blah'.
//~| ERROR: No overload matches this call.