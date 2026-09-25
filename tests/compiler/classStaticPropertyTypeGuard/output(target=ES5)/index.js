class A {
  static _a;
  get a() {
    if (A._a) {
      return A._a;
    }
    
    return A._a = 'helloworld';
  }
}