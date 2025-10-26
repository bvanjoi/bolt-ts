// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/errorForUsingPropertyOfTypeAsType02.ts`, Apache-2.0 License

namespace Test1 {
    function foo<T extends { abc: number }>(x: T) {
        let a: T.abc = x.abc;
        //~^ ERROR: Cannot access 'T.abc' because 'T' is a type, but not a namespace. Did you mean to retrieve the type of the property 'abc' in 'T' with 'T["abc"]'?
    }
}