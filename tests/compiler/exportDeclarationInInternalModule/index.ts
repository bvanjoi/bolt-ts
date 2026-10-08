// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportDeclarationInInternalModule.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: declaration


class Bbb {
}

class Aaa extends Bbb { }

namespace Aaa {
    export class SomeType { }
}

namespace Bbb {
    export class SomeType { }

    export * from Aaa;      // this line causes the nullref
    //~^ ERROR: Expected 'String'.
    //~| ERROR: Export declarations are not permitted in a namespace.
}

var a: Bbb.SomeType;
