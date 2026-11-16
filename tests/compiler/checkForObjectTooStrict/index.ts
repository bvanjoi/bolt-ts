// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/checkForObjectTooStrict.ts`, Apache-2.0 License

//@compiler-options: module=commonjs
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

namespace Foo {

    export class Object {
      //~^ ERROR: Class name cannot be 'Object' when targeting ES5 and above with module CommonJS.

    }

}

 

class Bar extends Foo.Object { // should work

    constructor () {

        super();

    }

}


class Baz extends Object {

    constructor () { // ERROR, as expected

        super();

    }

}