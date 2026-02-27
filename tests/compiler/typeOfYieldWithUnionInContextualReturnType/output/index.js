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