var result = canYouInferThis(() => ({a: {BLAH: 33},
b: (x) => {}}));
result.BLAH;
function goofus(f) {}
goofus((a) => ({dog() {
  return a
}}));
goofus((a) => ({dog: function () {
  return a
}}));