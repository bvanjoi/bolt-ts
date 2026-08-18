// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/indexedAccessNormalization.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function f1(mymap, k) {
  var elemofM = mymap[k];
  g(elemofM);
}
function f2(mymap, k, z) {
  var q1 = z;
  var q2 = z;
  var q3 = z;
}