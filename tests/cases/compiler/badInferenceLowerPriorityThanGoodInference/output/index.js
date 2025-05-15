
// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/badInferenceLowerPriorityThanGoodInference.ts`, Apache-2.0 License
//@ run-fail

var result = canYouInferThis(() => ({a: {BLAH: 33},
b: (x) => {}}));
result.BLAH;
// Repro from #26629
function goofus(f) {}
goofus((a) => ({dog() {
  return a
}}));
goofus((a) => ({dog: function () {
  return a
}}));