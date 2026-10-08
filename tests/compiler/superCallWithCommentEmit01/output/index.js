// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/superCallWithCommentEmit01.ts`, Apache-2.0 License
class A {
  constructor(text) {
    this.text = text}
}
class B extends A {
  constructor(// this is subclass constructor
  text) {super(text);}
}