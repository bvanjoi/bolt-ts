// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/intersectionTypeNormalization.ts`, Apache-2.0 License
//@compiler-options: target=es2015
// Identical ways of writing the same type
var x;
var x;
var x;// Identical ways of writing the same type

var y;
var y;
var y;// Identical ways of writing the same type

var z;
var z;
var z;
var z;// Repro from #9919

function getValueAsString(value) {
  if (value.kind === 'int') {
    return '' + value.num;
  }
  
  return value.str;
}
// Repro from #12535
var enums = {};
(function (enums) {

  var A = // ... elements omitted for the sake of clarity
  {};
  (function (A) {
  
    A[A['a1'] = 0] // ... elements omitted for the sake of clarity
    = 'a1'
    A[A['a2'] = 0] = 'a2'
    A[// ... elements omitted for the sake of clarity
    A['a3'] = 0] = 'a3'
    A[A['a75'] = 0] = 'a75'
    A[A['a76'] = 0] = 'a76'
    A[A['a77'] = 0] = 'a77'
  })(A);
  enums.A = A;
  
  var B = {};
  (function (B) {
  
    B[B['b1'] = 0] = 'b1'
    B[B['b2'] = 0] = 'b2'
    B[B['b86'] = 0] = 'b86'
    B[B['b87'] = 0] = 'b87'
  })(B);
  enums.B = B;
  
  var C = {};
  (function (C) {
  
    C[C['c1'] = 0] = 'c1'
    C[C['c2'] = 0] = 'c2'
    C[C['c210'] = 0] = 'c210'
    C[C['c211'] = 0] = 'c211'
  })(C);
  enums.C = C;
  
})(enums);
function foo(so) {
  var val = so;
  var isGenre = val.genreId;
  return isGenre;
}