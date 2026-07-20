declare namespace a {
  interface I {}
}
declare namespace c {
  import b = a.I;
  var x: b;
  
}
