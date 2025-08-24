// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/assignToObjectTypeWithPrototypeProperty.ts`, Apache-2.0 License

class XEvent {}
var p: XEvent = XEvent.prototype;
var x: {prototype: XEvent} = XEvent;