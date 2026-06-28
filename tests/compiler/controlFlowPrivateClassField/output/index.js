class Example {
  #test;
  constructor(test) {this.#test = test;}
  get test() {
    return this.#test;
  }
}
class Example2 {
  #test;
  constructor(test) {this.#test = test;}
  get test() {
    if (this.#test) {
      return this.#test;
    }
    
    return 0;
  }
}