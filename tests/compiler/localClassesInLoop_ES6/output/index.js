// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/localClassesInLoop_ES6.ts`, Apache-2.0 License
//@compiler-options: strict=false
//@compiler-options: target=ES6
'use strict';
var data = [];
for ( var x = 0; x < 2; ++x) {
  class C {}
  data.push(() => (C));
}
use(data[0]() === data[1]());