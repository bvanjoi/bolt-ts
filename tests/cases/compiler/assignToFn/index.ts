// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/assignToFn.ts`, Apache-2.0 License

module M {
    interface I {
	f(n:number):boolean;
    }

    var x:I={ f:function(n) { return true; } };

    x.f="hello";
    //~^ ERROR: Type 'string' is not assignable to type '(n: number) => boolean'.
}
