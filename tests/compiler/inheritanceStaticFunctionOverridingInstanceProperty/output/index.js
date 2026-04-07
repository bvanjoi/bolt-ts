class a {
  x
}
class b extends a {
  static x() {
    return new b().x
  }
}