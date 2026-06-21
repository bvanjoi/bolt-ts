// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/aliasBug.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

namespace foo {    
    export class Provide {
    }

    export namespace bar { export namespace baz {export class boo {}}}
}

import provide = foo;
import booz = foo.bar.baz;

var p = new provide.Provide();

function use() {
  var p1: provide.Provide; // error here, but should be okay
  var p2: foo.Provide;
  var p3:booz.bar;
  //~^ ERROR: Namespace 'foo.bar.baz' has no exported member 'bar'.
  var p22 = new provide.Provide();
}
