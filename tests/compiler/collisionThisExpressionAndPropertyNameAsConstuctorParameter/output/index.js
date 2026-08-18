// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionThisExpressionAndPropertyNameAsConstuctorParameter.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class Foo2 {
  constructor(_this) {var lambda = () => ((x) => (this));}
}
class Foo3 {
  constructor(_this) {var lambda = () => ((x) => (this));}
}
class Foo4 {
  constructor(_this) {var lambda = () => ((x) => (this));}
}
class Foo5 {
  constructor(_this) {var lambda = () => ((x) => (this));}
}