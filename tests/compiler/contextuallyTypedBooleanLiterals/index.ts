// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextuallyTypedBooleanLiterals.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration
//@run-fail

type Box<T> = {
    get: () => T,
    set: (value: T) => void
}

declare function box<T>(value: T): Box<T>;

const bn1 = box(0);  // Box<number>
const bn2: Box<number> = box(0);  // Ok

const bb1 = box(false);  // Box<boolean>
const bb2: Box<boolean> = box(false);  // Error, box<false> not assignable to Box<boolean>

interface Observable<T>
{
  (): T;
  (value: T): any;
}

declare function observable<T>(value: T): Observable<T>;

const x: Observable<boolean> = observable(false);
