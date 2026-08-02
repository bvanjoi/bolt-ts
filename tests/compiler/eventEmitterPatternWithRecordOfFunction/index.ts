// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/eventEmitterPatternWithRecordOfFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

interface A {
    emit(event: string, ...args: any[]): boolean;
}

type Args<F> = F extends (...args: infer A) => void ? A : never;

type EventMap = Record<string, Function>;

interface B<M extends EventMap> extends A {
    emit<Event extends keyof M>(event: Event, ...args: Args<M[Event]>): boolean;
}