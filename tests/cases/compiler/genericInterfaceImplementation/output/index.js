
class None {
  get() {
    throw null
  }
  flatten() {
    return new None()
  }
}