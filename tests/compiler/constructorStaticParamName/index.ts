// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/constructorStaticParamName.ts`, Apache-2.0 License

class test {
    constructor (static) { }
    //~^ ERROR: Identifier expected. 'static' is a reserved word in strict mode. Class definitions are automatically in strict mode.
}


class X { st\u0061tic y() { } }
//~^ ERROR: Keywords cannot contain escape characters.

const \u0061 = 1;
const b = a;
