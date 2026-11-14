// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionRestParameterClassConstructor.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
// Constructors
class c1 {
  constructor(_i, ...restParameters) {//_i is error
  var _i = 10;// no error
    }
}
class c1NoError {
  constructor(_i) {// no error
  var _i = 10;// no error
    }
}
class c2 {
  constructor(...restParameters) {var _i = 10;// no error
    }
}
class c2NoError {
  constructor() {var _i = 10;// no error
    }
}
class c3 {
  constructor(_i, ...restParameters) {//_i is error
  
    var _i = 10;// no error
    
    this._i = _i
    
    }
}
class c3NoError {
  constructor(_i// no error
  ) {
    var _i =// no error
     10;
    this._i = _i
    }
}// No error - no code gen
// no error

class c5 {
  // no codegen no error
  // no codegen no error
  constructor(_i, ...rest) {// error
  var _i;// no error
    }
}
class c5NoError {
  // no error
  // no error
  constructor(_i) {// no error
  var _i;// no error
    }
}// no codegen no error
// no codegen no error
// no error
// no error
