class C {
  static g(t) {}
}
C.g(C.g);
C.g((a) => {
  a(a);
});