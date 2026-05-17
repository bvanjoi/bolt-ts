// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionSubtypingOfVarArgs2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class EventBase {
    private _listeners: { (...args: any[]): void; }[] = [];

    add(listener: (...args: any[]) => void): void {
        this._listeners.push(listener);
    }
}

class StringEvent extends EventBase {
    add(listener: (items: string, moreitems: number) => void ) {
        super.add(listener);
    }
}
