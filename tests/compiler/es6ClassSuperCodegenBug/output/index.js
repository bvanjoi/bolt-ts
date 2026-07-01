class A {
  constructor(str1, str2) {}
}
class B extends A {
  constructor() {if (true) {
      super('a1', 'b1');
    } else {
      super('a2', 'b2');
    }
    }
}