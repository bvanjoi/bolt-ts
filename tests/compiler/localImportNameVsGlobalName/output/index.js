var Keyboard = {};
(function (Keyboard) {

  var Key = {};
  (function (Key) {
  
    Key[Key['UP'] = 0] = 'UP'
    Key[Key['DOWN'] = 0] = 'DOWN'
    Key[Key['LEFT'] = 0] = 'LEFT'
    Key[Key['RIGHT'] = 0] = 'RIGHT'
  })(Key);
  Keyboard.Key = Key;
  
})(Keyboard);
var App = {};
(function (App) {

  var Key = Keyboard.Key
  
  function foo(key) {}
  App.foo = foo;
  
  foo(Key.UP);
  
  foo(Key.DOWN);
  
  foo(Key.LEFT);
  
})(App);