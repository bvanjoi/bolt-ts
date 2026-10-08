// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/asyncFunctionContextuallyTypedReturns.ts`, Apache-2.0 License
//@compiler-options: target=es6
//@compiler-options: strict
//@run-fail
f((v) => (v ? [0] : Promise.reject()));
f(async (v) => (v ? [0] : Promise.reject()));
g((v) => (v ? 'contextuallyTypable' : Promise.reject()));
g(async (v) => (v ? 'contextuallyTypable' : Promise.reject()));
h((v) => (v ? (abc) => {} : Promise.reject()));
h(async (v) => (v ? (def) => {} : Promise.reject()));
// repro from #29196
var increment = async (num, str) => ((a) => (a.length));
var increment2 = async (num, str) => ((a) => (a.length));