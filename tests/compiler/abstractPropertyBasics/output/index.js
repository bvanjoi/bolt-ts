class B {
  
  
  
}
class C extends B {
  get prop() {
    return 'foo';
  }
  set prop(v) {}
  raw = 'edge';
  ro = 'readonly please';
  readonlyProp;
  // don't have to give a value, in fact
  m() {}
}