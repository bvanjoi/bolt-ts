// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/downlevelLetConst19.ts`, Apache-2.0 License
//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@[target=ES2015]  run-fail
'use strict';
var x;
function a() {
  {
    var x;
    use(x);
    function b() {
      //~[target=ES5]^ ERROR: Function declarations are not allowed inside blocks in strict mode when targeting 'ES5'.
      {
        var x;
        use(x);
      }
      use(x);
    }
  }
  use(x);
}
use(x);