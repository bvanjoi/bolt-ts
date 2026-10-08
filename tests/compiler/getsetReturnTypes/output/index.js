// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/getsetReturnTypes.ts`, Apache-2.0 License
function makePoint(x) {
  return {
      get x() {
      return x;
    }    
  };
}
;
var x = makePoint(2).x;
var y = makePoint(2).x;