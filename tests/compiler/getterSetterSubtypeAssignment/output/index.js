class NumberOrUndefined {
  _x;
  get x() {
    return this._x ?? 0;
  }
  set x(value) {
    this._x = value;
  }
  additionAssignment() {
    this.x += 1;
  }
  subtractionAssignment() {
    this.x -= 1;
  }
  multiplicationAssignment() {
    this.x *= 1;
  }
  divisionAssignment() {
    this.x /= 1;
  }
}
var numberOrUndefined = new NumberOrUndefined();
numberOrUndefined.x += 1;
numberOrUndefined.x -= 1;
numberOrUndefined.x *= 1;
numberOrUndefined.x /= 1;
class NumberOrString {
  _x = 0;
  get x() {
    return typeof this._x === 'number' ? this._x : Number(this._x);
  }
  set x(value) {
    this._x = value;
  }
  additionAssignmentNumber() {
    this.x += 1;
  }
  additionAssignmentString() {
    this.x += '1';
  }
  subtractionAssignment() {
    this.x -= 1;
  }
  multiplicationAssignment() {
    this.x *= 1;
  }
  divisionAssignment() {
    this.x /= 1;
  }
}
var numberOrString = new NumberOrString();
numberOrString.x += 1;
numberOrString.x += '1';
numberOrString.x -= 1;
numberOrString.x *= 1;
numberOrString.x /= 1;
class NumberOrObject {
  _x = 0;
  get x() {
    return typeof this._x === 'number' ? this._x : this._x.bar;
  }
  set x(value) {
    this._x = value;
  }
  additionAssignment() {
    this.x += 1;
  }
  subtractionAssignment() {
    this.x -= 1;
  }
  multiplicationAssignment() {
    this.x *= 1;
  }
  divisionAssignment() {
    this.x /= 1;
  }
}
var numberOrObject = new NumberOrObject();
numberOrObject.x += 1;
numberOrObject.x -= 1;
numberOrObject.x *= 1;
numberOrObject.x /= 1;