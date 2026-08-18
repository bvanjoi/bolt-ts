// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyModule.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

declare namespace Module {
    interface Interface {
        // Should return error for implicit any on return type.
        new ();
        //~^ ERROR: Construct signature, which lacks return-type annotation, implicitly has an 'any' return type.
    }

    class Class {
        // Should return error for implicit `any` on parameter.
        public f(x): any;
        //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
        public g(x: any);
        //~^ ERROR: 'g', which lacks return-type annotation, implicitly has an 'any' return type.

        // Should not return error at all.
        private h(x);
    }

    // Should return error for implicit any on return type.
    function f(x: number);
    //~^ ERROR: 'f', which lacks return-type annotation, implicitly has an 'any' return type.
}
