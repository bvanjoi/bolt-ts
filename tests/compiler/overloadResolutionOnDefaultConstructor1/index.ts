// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/overloadResolutionOnDefaultConstructor1.ts`, Apache-2.0 License

class Bar {
    public clone() {
        return new Bar(0);
        //~^ ERROR: Expected 0 arguments, but got 1.
    }
}

