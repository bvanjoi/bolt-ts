// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericPrototypeProperty2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
class BaseEvent {
  target;
}
class MyEvent extends BaseEvent {
  target;
}
class BaseEventWrapper {
  t;
}
class MyEventWrapper extends BaseEventWrapper {
  t;
// any satisfies constraint and passes assignability check between 'target' properties
}