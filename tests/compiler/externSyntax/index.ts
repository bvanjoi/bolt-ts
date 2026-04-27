// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/externSyntax.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare var v;
declare namespace M {
    export class D {
        public p;
    }
    export class C {
        public f();
        public g() { } // error body
        //~^ ERROR: An implementation cannot be declared in ambient contexts.
    }
}