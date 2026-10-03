class Chain {
  constructor(value) {
    this.value = value}
  then(cb) {
    var result = cb(this.value);
    var z = this.then((x) => (result)).then((x) => ('abc')).then((x) => (x.length));
    return new Chain(result);
  }
}