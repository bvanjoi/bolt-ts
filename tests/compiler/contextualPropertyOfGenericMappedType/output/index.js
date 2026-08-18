// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/contextualPropertyOfGenericMappedType.ts`, Apache-2.0 License
//@compiler-options: target=esnext
//@compiler-options: noImplicitAny
f({
  data: 0  
}, {
  data(value, key) {}  
});