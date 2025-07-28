// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/noRepeatedPropertyNames.ts`, Apache-2.0 License

//@compiler-options: strict

const first = { a: 1, a: 2 };
//~^ ERROR: An object literal cannot have multiple properties with the same name.
class C {
    m() {
        const second = { a: 1, a: 2 };
        //~^ ERROR: An object literal cannot have multiple properties with the same name.
        return second.a;
    }
}