// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/this_inside-enum-should-not-be-allowed.ts`, Apache-2.0 License

//@compiler-options: target=es2015

enum TopLevelEnum {
    ThisWasAllowedButShouldNotBe = this // Should not be allowed
        //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
        //~| ERROR: 'this' cannot be referenced in current location.
}

namespace ModuleEnum {
    enum EnumInModule {
        WasADifferentError = this // this was handled as if this was in a module
        //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
        //~| ERROR: 'this' cannot be referenced in current location.
    }
}