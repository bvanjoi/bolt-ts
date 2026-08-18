// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/overloadResolutionOverNonCTObjectLit.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var Bugs = {};
(function (Bugs) {

  function bug3() {
    var tokens = [];
    tokens.push({
          startIndex: 1,
      type: '',
      bracket: 3      
    });
    tokens.push(({
          startIndex: 1,
      type: '',
      bracket: 3,
      state: null,
      length: 10      
    }));
  }
  
})(Bugs);