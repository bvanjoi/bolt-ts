// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceExtendsClass1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class Control {
  state;
}
class Button extends Control {
  select() {}
}
class TextBox extends Control {
  select() {}
}
class Image extends Control {}
class Location {
  select() {}
}