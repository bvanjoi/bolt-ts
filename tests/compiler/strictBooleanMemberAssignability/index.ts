// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/strictBooleanMemberAssignability.ts`, Apache-2.0 License

//@compiler-options: strict

class Abc {
    def: boolean
    constructor() {
        this.def = true
        this.def = false
        this.def = 42;
        //~^ ERROR: Type 'number' is not assignable to type 'boolean'.
    }
}