
// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/abstractPropertyBasics.ts`, Apache-2.0 License
class B {
  
  
  
  
  
  
}
class C extends B {
  get prop() {
    return "foo"
  }
  set prop(v) {}
  raw = "edge"
  ro = "readonly please"
  readonlyProp
  // don't have to give a value, in fact
  m() {}
}