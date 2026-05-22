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
    return +a
  }  
});
var kind = 'a';
invoke({
  kind,
  method(a) {
    return +a
  }  
});