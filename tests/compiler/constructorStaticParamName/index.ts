// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constructorStaticParamName.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class test {
    constructor (static) { }
    //~^ ERROR: Identifier expected. 'static' is a reserved word in strict mode.
}


class X { st\u0061tic y() { } }
//~^ ERROR: Keywords cannot contain escape characters.

const \u0061 = 1;
const b = a;
