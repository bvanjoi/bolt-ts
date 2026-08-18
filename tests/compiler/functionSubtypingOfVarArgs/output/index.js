// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionSubtypingOfVarArgs.ts`, Apache-2.0 License
//@compiler-options: target=es2015
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