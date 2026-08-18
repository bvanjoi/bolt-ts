// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/instanceofTypeAliasToGenericClass.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function fn(o) {
  return o instanceof TableClass;
}
function fn2(o) {
  return o instanceof TableClass;
}

o instanceof TableClass;