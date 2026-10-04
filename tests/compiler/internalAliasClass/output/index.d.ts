declare namespace a {
  class c {}
}
declare namespace c {
  import b = a.c;
  var x: b;
}
