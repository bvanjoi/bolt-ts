// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionSuperAndPropertyNameAsConstuctorParameter.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class a {}
class b1 extends a {
  constructor(_super) {// should be error
    super();}
}
class b2 extends a {
  constructor(_super) {// should be error
    super();}
}
class b3 extends a {
  // no code gen - no error
  constructor// no code gen - no error
  (_super) {// should be error
    super();}
}
class b4 extends a {
  // no code gen - no error
  constructor// no code gen - no error
  (_super) {// should be error
    super();}
}