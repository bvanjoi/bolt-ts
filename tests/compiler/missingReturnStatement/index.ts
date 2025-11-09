// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/missingReturnStatement.ts`, Apache-2.0 License

namespace Test {
    export class Bug {
        public foo():string {
        //~^ ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.
        }
    }    
}
