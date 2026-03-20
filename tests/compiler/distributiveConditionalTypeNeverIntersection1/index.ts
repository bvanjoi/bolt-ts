// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/distributiveConditionalTypeNeverIntersection1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type IsNumber<T> = T extends number ? true : false;

type Conflicted = { x: true } & { x: false };

type Ex1 = IsNumber<Conflicted>; // never
type Ex2 = IsNumber<"OEEE" | Conflicted>; // false
type Ex3 = IsNumber<1 | Conflicted>; // true
