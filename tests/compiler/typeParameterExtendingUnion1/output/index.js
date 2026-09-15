class Animal {
  run() {}
}
class Cat extends Animal {
  meow;
}
class Dog extends Animal {
  woof;
}
function run(a) {
  a.run();
}
function f(a) {
  a.run();
  run(a);
}