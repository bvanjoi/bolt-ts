class BaseClass {
  fake() {
    throw new Error('')
  }
}
class Klass extends BaseClass {
  child = true;
}

m.child;