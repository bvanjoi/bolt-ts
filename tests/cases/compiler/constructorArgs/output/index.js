
class Super {
  constructor(value) {}
}
class Sub extends Super {
  constructor(options) {
    super(options.value);
    this.options = options
    }
}