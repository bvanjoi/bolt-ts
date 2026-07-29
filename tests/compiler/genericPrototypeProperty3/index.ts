// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericPrototypeProperty3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class BaseEvent {
    target: {};
    //~^ ERROR: Property 'target' has no initializer and is not definitely assigned in the constructor.
}

class MyEvent<T> extends BaseEvent { // T is instantiated to any in the prototype, which is assignable to {}
    target: T;
    //~^ ERROR: Property 'target' in type 'MyEvent<T, MyEvent>' is not assignable to the same property in base type 'BaseEvent<MyEvent>'.
    //~| ERROR: Property 'target' has no initializer and is not definitely assigned in the constructor.
}
class BaseEventWrapper {
    t: BaseEvent;
    //~^ ERROR: Property 't' has no initializer and is not definitely assigned in the constructor.
}

class MyEventWrapper extends BaseEventWrapper {
    t: MyEvent<any>;
    //~^ ERROR: Property 't' has no initializer and is not definitely assigned in the constructor.
}