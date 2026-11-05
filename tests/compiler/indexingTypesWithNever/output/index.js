// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexingTypesWithNever.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail
// Should be never but without an error
// Should be never but without an error
// Should be never
var result3 = genericFn1({
  c: 'ctest',
  d: 'dtest'  
});
// Should be never
var result4 = genericFn2({
  e: 'etest',
  f: 'ftest'  
});
// Should be never
var result5 = genericFn3({
  g: 'gtest',
  h: 'htest'  
}, 'g', 'h');// 'g' & 'h' will reduce to never



var result6 = obj[key];// Expanded examples from https://github.com/Microsoft/TypeScript/issues/21988
// expect 'a' | 'b'
// expect 'a'
// expect never
// expect never




// expect { a: string; b: number }
// expect { a: string; }
// expect {}
// expect {}




// expect 'a' | 'b'
// expect 'a'
// expect never
// expect never




// expect { a?: string | undefined; b?: number | undefined }
// expect { a?: string | undefined; }
// expect {}
// expect {}




// Repro from #23005
// "x" | "y"
// "x"
