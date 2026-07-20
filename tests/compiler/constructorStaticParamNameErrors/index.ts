// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constructorStaticParamNameErrors.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

'use strict'
// static as constructor parameter name should give error if 'use strict'
class test {
    constructor (static) { }
    //~^ ERROR: Identifier expected. 'static' is a reserved word in strict mode.
}
