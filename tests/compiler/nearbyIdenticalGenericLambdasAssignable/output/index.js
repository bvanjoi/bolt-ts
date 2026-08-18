// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nearbyIdenticalGenericLambdasAssignable.ts`, Apache-2.0 License
//@compiler-options: target=es2015

var fB = () => ({
  v: ''  
});
var fC = () => ({});
accA(fA);
accA(fB);
accA(fC);
accB(fA);
accB(fB);
accB(fC);
accC(fA);
accC(fB);
accC(fC);
accL(fA);
accL(fB);
accL(fC);