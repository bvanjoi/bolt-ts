// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/typeParameterExtendingUnion1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
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