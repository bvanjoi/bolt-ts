// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/restParameterAssignmentCompatibility.ts`, Apache-2.0 License
//@compiler-options: strict=false
class T {
  m(...p3) {}
}
class S {
  m(p1, p2) {}
}
var t;
var s;
t = s;
class T1 {
  m(p1, p2) {}
}
var t1;
t1 = s;