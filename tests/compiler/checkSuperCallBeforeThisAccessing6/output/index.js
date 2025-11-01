class Base {
  constructor(...arg) {}
}
class Super extends Base {
  constructor() {(() => (this));super();}
}