// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/interfaceInReopenedModule.ts`, Apache-2.0 License
var m = {};
(function (m) {

})(m);
// In second instance of same module, exported interface is not visible

(function (m) {

  
  
  class n {
    n
  }
  m.n = n;
  
})(m);