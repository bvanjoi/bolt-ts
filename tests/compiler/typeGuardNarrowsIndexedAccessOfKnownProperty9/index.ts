// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeGuardNarrowsIndexedAccessOfKnownProperty9.ts`, Apache-2.0 License

//@compiler-options: noUnusedLocals
//@compiler-options: strict
//@compiler-options: target=es2015

class C1 {
    private a = "a"; // ok
    private b = "b"; // ok

    private c = "c"; // error unused prop
    //~^ ERROR: 'c' is declared but its value is never read.
    private d = "d"; // error unused prop
    //~^ ERROR: 'd' is declared but its value is never read.

    getValue(key: "a" | "b") {
        return this[key];
    }
}
