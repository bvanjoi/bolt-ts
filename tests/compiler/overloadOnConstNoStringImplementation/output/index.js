// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/overloadOnConstNoStringImplementation.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
function x2(a, cb) {
  cb('hi');
  cb('bye');
  var hm = 'hm';
  cb(hm);
  // should this work without a string definition?
  cb('uh');
  cb(1);
}
var cb = (x) => (1);
x2(1, cb);
// error
x2(1, (x) => (1))// error
;
x2(1, (x) => (1));