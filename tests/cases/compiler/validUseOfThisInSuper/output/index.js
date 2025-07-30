class Base {
  constructor(b) {
    this.b = b}
}
class Super extends Base {
  constructor() {super((() => (this))());}
}