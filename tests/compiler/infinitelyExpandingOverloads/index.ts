// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/infinitelyExpandingOverloads.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface KnockoutSubscription2<T> {
    target: KnockoutObservableBase2<T>;
}
interface KnockoutObservableBase2<T> {
    subscribe(callback: (newValue: T) => void, target?: any, topic?: string): KnockoutSubscription2<T>;
}
interface ValidationPlacement2<TValue> {
    initialize(validatable: Validatable2<TValue>): void;
}
interface Validatable2<TValue> {
    validators: KnockoutObservableBase2<Validator2<TValue>>;
}
class Validator2<TValue> {
    private _subscription: KnockoutSubscription2<TValue>;
    //~^ ERROR: Property '_subscription' has no initializer and is not definitely assigned in the constructor.
}
class ViewModel<TValue> {
    public validationPlacements: Array<ValidationPlacement2<TValue>> = new Array<ValidationPlacement2<TValue>>();
}
class Widget<TValue> {
    constructor(viewModelType: new () => ViewModel<TValue>); // Shouldnt error on this overload
    constructor(viewModelType: new () => ViewModel<TValue>) {
    }
    public get options(): ViewModel<TValue> {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'ViewModel<TValue>'.
    }
}