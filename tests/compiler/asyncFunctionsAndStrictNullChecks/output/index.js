
async function sample(promise) {
  var number = await promise;
}
async function sample2(x) {
  var x1 = await resolve1(x);
  var x2 = await resolve2(x);
}