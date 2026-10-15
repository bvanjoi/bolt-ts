// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualSignatureInstatiationCovariance.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
var f2;
var g2;
g2 = f2;// While neither Animal nor TallThing satisfy the constraint, T is at worst a Giraffe and compatible with both via covariance.

var h2;
h2 = f2;