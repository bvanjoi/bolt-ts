// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/circularContextualMappedType.ts`, Apache-2.0 License

//@ run-fail

type Func<T> = () => T;

type Mapped<T> = { [K in keyof T]: Func<T[K]> };

declare function reproduce(options: number): void;
declare function reproduce<T>(options: Mapped<T>): T
reproduce(42);

// TODO:
// reproduce('42');
// reproduce({
//   name:   () => { return 123 }
// });

// reproduce({
//   name() { return 123 }
// });

// reproduce({
//   name: function () { return 123 }
// });
