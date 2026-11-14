// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/promiseChaining.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class Chain {
  constructor(value) {
    this.value = value}
  then(cb) {
    var result = cb(this.value);
    // should get a fresh type parameter which each then call
    var z = this.then((x) => (result))/*S*/ .then((x) => ('abc'))/*string*/ .then((x) => (x.length))/*number*/ ;// No error
    
    return new Chain(result);
  }
}