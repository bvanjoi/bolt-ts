// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/checkInfiniteExpansionTermination2.ts`, Apache-2.0 License

interface IObservable<T> {
  n: IObservable<T[]>;
}
interface ISubject<T> extends IObservable<T> { }

declare function combineLatest<TOther>(x: IObservable<TOther>[]): void;
declare function combineLatest(): void;

function fn<T>() {
  var values: ISubject<any>[] = [];
  // Hang when using <T>, but not <any>
  combineLatest<T>(values);
}
