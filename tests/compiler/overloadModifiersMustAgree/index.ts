// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadModifiersMustAgree.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: module=commonjs

class baz {
    public foo();
    //~^ ERROR: Overload signatures must all be public, private or protected.
    private foo(bar?: any) { } // error - access modifiers do not agree
}

declare function bar();
//~^ ERROR: Overload signatures must all be ambient or non-ambient.
export function bar(s: string);
//~^ ERROR: Overload signatures must all be exported or non-exported.
function bar(s?: string) { }

interface I {
    foo? ();
    foo(s: string);
    //~^ ERROR: Overload signatures must all be optional or required.
}

