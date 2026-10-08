declare namespace a {
  var x: number;
}
declare namespace c {
  import b = a.x;
  var bVal: number;
}
