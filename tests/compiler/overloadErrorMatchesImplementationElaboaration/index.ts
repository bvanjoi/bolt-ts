// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/overloadErrorMatchesImplementationElaboaration.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class EventAggregator
{
    publish(event: string, data?: any): void;
    publish<T>(event: T): void {}
}

declare var ea: EventAggregator;
ea.publish([1,2,3]);
//~^ ERROR: Argument of type 'number[]' is not assignable to parameter of type 'string'.