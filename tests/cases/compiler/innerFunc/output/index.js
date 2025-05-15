// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/innerFunc.ts`, Apache-2.0 License
function salt() {
  function pepper() {
    return 5
  }
  return pepper()
}
var M = {};
(function (M) {

  function tungsten() {
    function oxygen() {
      return 6
    }
    
    return oxygen()
  }
  M.tungsten = tungsten;
  
})(M);