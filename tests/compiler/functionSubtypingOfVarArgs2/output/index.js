class EventBase {
  _listeners = [];
  add(listener) {
    this._listeners.push(listener);
  }
}
class StringEvent extends EventBase {
  add(listener) {
    super.add(listener);
  }
}