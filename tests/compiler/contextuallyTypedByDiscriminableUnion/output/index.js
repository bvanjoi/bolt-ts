// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextuallyTypedByDiscriminableUnion.ts`, Apache-2.0 License
//@compiler-options: target=esnext
//@compiler-options: noImplicitAny
function invoke(item) {
  if (item.kind === 'a') {
    item.method('');
  } else {
    item.method(42);
  }
  
}
invoke({
  kind: 'a',
  method(a) {
    return +a;
  }  
});
var kind = 'a';
invoke({
  kind,
  method(a) {
    return +a;
  }  
});