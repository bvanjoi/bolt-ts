// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionSuperAndPropertyNameAsConstuctorParameter.ts`, Apache-2.0 License
class a {}
class b1 extends a {
  constructor(_super) {super();}
}
class b2 extends a {
  constructor(_super) {super();}
}
class b3 extends a {
  constructor(_super) {super();}
}
class b4 extends a {
  constructor(_super) {super();}
}