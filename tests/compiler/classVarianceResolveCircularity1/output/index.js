// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classVarianceResolveCircularity1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class Bar {
  num;
  Value = callme(this).num;
  Field = callme(this).num;
}