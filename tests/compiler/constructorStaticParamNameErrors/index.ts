// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/constructorStaticParamNameErrors.ts`, Apache-2.0 License

'use strict'
// static as constructor parameter name should give error if 'use strict'
class test {
    constructor (static) { }
    //~^ ERROR: Identifier expected. 'static' is a reserved word in strict mode.
    //~| ERROR: Identifier expected. 'static' is a reserved word in strict mode. Class definitions are automatically in strict mode.
}