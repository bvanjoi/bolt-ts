// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/downlevelLetConst15.ts`, Apache-2.0 License
//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@run-fail
'use strict';
var x = 10;
var z0, z1, z2, z3;
{
  var x = 20;
  use(x);
  var [z0] = [1];
  use(z0);
  var [{a: z1}] = [{
      a: 1    
  }];
  use(z1);
  var {a: z2} = {
      a: 1    
  };
  use(z2);
  var {a: {b: z3}} = {
      a: {
          b: 1      
    }    
  };
  use(z3);
}
use(x);
use(z0);
use(z1);
use(z2);
use(z3);
var z6;
var y = true;
{
  var y = '';
  var [z6] = [true];
  {
    var y = 1;
    var {a: z6} = {
          a: 1      
    };
    use(y);
    use(z6);
  }
  use(y);
  use(z6);
}
use(y);
use(z6);
var z = false;
var z5 = 1;
{
  var z = '';
  var [z5] = [5];
  {
    var _z = 1;
    var {a: _z5} = {
          a: 1      
    };
    // try to step on generated name
    use(_z);
  }
  use(z);
}
use(y);