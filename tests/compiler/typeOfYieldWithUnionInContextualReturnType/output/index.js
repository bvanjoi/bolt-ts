{
  function f(d) {
    var a = d;
  }
  var a0 = true;
  async function* e() {
    yield true;
  }
}
var syncFactory = function* () {
  var name = '';
  while (!name) {
    name = yield 'What is your name?';
  }
  return `That's the end of the game, ${name}`
};
var asyncFactory = function* () {
  var name = '';
  while (!name) {
    name = yield 'What is your name?';
  }
  return `That's the end of the game, ${name}`
};
var looserSyncFactory = function* () {
  var name = '';
  while (!name) {
    name = yield 'What is your name?';
  }
  return `That's the end of the game, ${name}`
};
var looserAsyncFactory = function* () {
  var name = '';
  while (!name) {
    name = yield 'What is your name?';
  }
  return `That's the end of the game, ${name}`
};