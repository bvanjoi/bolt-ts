
// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nearbyIdenticalGenericLambdasAssignable.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@run-fail
var fB = () => ({
  v: ''  
});
var fC = () => ({});// Hover display is identical on all of these

// These should all be OK, every type is identical
accA(fA);
accA(fB);
accA(fC);
//             ~~ previously an error
accB(fA);
accB(fB);
accB(fC);
//             OK
accC(fA);
accC(fB);
accC(fC);
//             ~~ previously an error
accL(fA);
accL(fB);
accL(fC);