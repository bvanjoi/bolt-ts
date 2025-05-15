
// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeParameterOrderReversal.ts`, Apache-2.0 License
// Only difference here is order of type parameters
function uFirst(x) {}
function tFirst(x) {}
var z = null;
// Both of these should be allowed
uFirst(z);
tFirst(z);