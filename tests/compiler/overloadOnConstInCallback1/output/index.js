// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadOnConstInCallback1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
class C {
  // error
  x1(a, callback) {
    callback('hi');
    callback('bye');
    var hm = 'hm';
    callback(hm);
  }
}