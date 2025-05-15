// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/baseTypeOrderChecking.ts`, Apache-2.0 License
var someVariable;
class Class1 {}
class Class2 extends Class1 {}
class Class3 {
  memberVariable
}
class Class4 extends Class3 {}