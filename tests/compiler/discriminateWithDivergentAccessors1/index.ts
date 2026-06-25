// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/discriminateWithDivergentAccessors1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type WeirdoBox<T> =
  | { get done(): false; set done(v: T | null) }
  | { get done(): true; set done(v: T | null); value: T };

declare const weirdoBox: WeirdoBox<number>;

if (weirdoBox.done) {
  weirdoBox.value;
}

type WeirdoBox2<T> =
  | { get done(): false; set done(v: T | null) }
  | { get done(): true; set done(v: T | null); value: T }
  | { get done(): true; set done(v: T | null | undefined); value: number };

declare const weirdoBox2: WeirdoBox2<string>;

if (weirdoBox2.done) {
  weirdoBox2.value;
}
