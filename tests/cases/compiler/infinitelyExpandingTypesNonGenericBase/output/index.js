// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/infinitelyExpandingTypesNonGenericBase.ts`, Apache-2.0 License
class Functionality {
  property
}
class Base {}
class A extends Base {
  options
}


function o(type) {}
o(A);