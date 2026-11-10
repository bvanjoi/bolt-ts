// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/iterableWithNeverAsUnionMember.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: noEmit
//@[target=ES5]     compiler-options: target=es5
//@[target=ESNext]  compiler-options: target=esnext

declare const o1: { a: "foo" } & { a: "bar" };
const [el1] = o1; // error
//~[target=ESNext]^ ERROR: Type 'never' must have a '[Symbol.iterator]()' method that returns an iterator.

// https://github.com/microsoft/TypeScript/issues/62462
declare var x: number[] | ({ t: "a" } & { t: "b" });
let [el2] = x; // ok

for (const elem of x) { // ok
  elem.toFixed();
}

type Shape =
  | { kind: "circle"; radius: number }
  | { kind: "rectangle"; width: number; height: number };

type Circle = Shape & { kind: "circle" };

function doStuffWithCircle(arg: Circle | [Circle, (newValue: Circle) => void]) {
  if (Array.isArray(arg)) {
    let [value, setValue] = arg; // ok
  }
}

function f1<T extends { a: "foo" } & { a: "bar" }>(x: T) {
  let [y] = x; // error
//~[target=ESNext]^ ERROR: Type 'T' must have a '[Symbol.iterator]()' method that returns an iterator.
}

declare const o2: ({ a: "foo" } & { a: "bar" }) | ({ b: "qwe" } & { b: "rty" });
const [el3] = o2; // error
//~[target=ESNext]^ ERROR: Type 'never' must have a '[Symbol.iterator]()' method that returns an iterator.
