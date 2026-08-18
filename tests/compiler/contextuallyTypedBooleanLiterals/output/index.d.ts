type Box<T> = {
  get: () => T;
  set: (value: T) => void;
};
declare function box<T>(value: T): Box<T>;
declare var bn1: Box<number>;
declare var bn2: Box<number>;
declare var bb1: Box<boolean>;
declare var bb2: Box<boolean>;
interface Observable<T> {
  (): T;
  (value: T): any;
}
declare function observable<T>(value: T): Observable<T>;
declare var x: Observable<boolean>;
