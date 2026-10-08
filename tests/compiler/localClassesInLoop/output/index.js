// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/localClassesInLoop.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail
'use strict';
var data = [];
for ( var x = 0; x < 2; ++x) {
  class C {}
  data.push(() => (C));
}
use(data[0]() === data[1]());