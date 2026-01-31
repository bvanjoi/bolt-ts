class A {
  constructor(p) {}
}
class B extends A {
  constructor() {super({
          test: () => (this.someMethod())      
    });}
  someMethod() {}
}