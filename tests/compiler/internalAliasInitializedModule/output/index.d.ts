declare namespace a {
  namespace b {
    class c {}
  }
}
declare namespace c {
  import b = a.b;
  var x: b.c;
}
