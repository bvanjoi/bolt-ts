// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classExtendingAbstractClassWithMemberCalledTheSameAsItsOwnTypeParam.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit
//@compiler-options: lib=[esnext]

interface IObserver {
	handleChange<T, TChange>(observable: IObservable<T, TChange>, change: TChange): void;
}

interface IObservable<T, TChange = unknown> {
	get(): T;
	readonly TChange: TChange;
}

export interface IReader {
	readObservable<T>(observable: IObservable<T, any>): T;
}

export abstract class ConvenientObservable<T, TChange> implements IObservable<T, TChange> {
	get TChange(): TChange { return null!; }
	public abstract get(): T;
}

export abstract class BaseObservable<T, TChange = void> extends ConvenientObservable<T, TChange> {
	protected readonly observers = new Set<IObserver>();
}

abstract class B<T> {
  get T(): number { return 42 }
}

abstract class A<T> extends B<T> {}