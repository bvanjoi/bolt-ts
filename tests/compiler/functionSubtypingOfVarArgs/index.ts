// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionSubtypingOfVarArgs.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class EventBase {
    private _listeners = [];

    add(listener: (...args: any[]) => void): void {
        this._listeners.push(listener);
    }
}

class StringEvent extends EventBase { // should work
    add(listener: (items: string) => void ) { // valid, items is subtype of args
        super.add(listener);
    }
}
