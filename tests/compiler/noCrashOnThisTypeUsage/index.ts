// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noCrashOnThisTypeUsage.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

interface IListenable {
    changeListeners: Function[] | null
    observe(handler: (change: any, oldValue?: any) => void, fireImmediately?: boolean): void
}

function notifyListeners<T>(listenable: IListenable, change: T) {
}

export class ObservableValue<T> {
    constructor(
        public value: T
    ) {
        const newValue: T = value;
        const oldValue: any = null;
        notifyListeners(this, {
            type: "update",
            object: this,
            newValue,
            oldValue
        });
    }
    changeListeners: Function[] | null = [];
    observe(handler: (change: any, oldValue?: any) => void, fireImmediately?: boolean) {}
}