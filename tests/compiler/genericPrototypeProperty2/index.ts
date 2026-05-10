// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericPrototypeProperty2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface EventTarget { x } 
class BaseEvent {
    target: EventTarget;
}

class MyEvent<T extends EventTarget> extends BaseEvent {
    target: T;
}
class BaseEventWrapper {
    t: BaseEvent;
}

class MyEventWrapper extends BaseEventWrapper {
    t: MyEvent<any>; // any satisfies constraint and passes assignability check between 'target' properties
}