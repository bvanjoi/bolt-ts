class foo {
  
  
  bar(foo) {
    return "foo"
  }
  n() {
    var foo = this.bar()
    foo = this.bar("test");
  }
}