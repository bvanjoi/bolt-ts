class Base {
  constructor(func) {}
}
class Super extends Base {
  constructor() {super((() => (this)));}
}