// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/returnValueInSetter.ts`, Apache-2.0 License

class f {
    set x(value) {
        return null; // Should be an error
        //~^ ERROR: Setters cannot return a value.
    }
}

