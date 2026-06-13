declare namespace m {
  export class c {}
}
declare namespace m1 {
  import x = m.c;
  var d: m.c;
  
}
declare namespace m2 {
  export import x = m.c;
  var d: m.c;
  
}
