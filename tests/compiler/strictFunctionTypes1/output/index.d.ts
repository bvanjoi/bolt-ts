declare function f1<T>(f1: (x: T) => void, f2: (x: T) => void): (x: T) => void;
declare function f2<T>(obj: T, f1: (x: T) => void, f2: (x: T) => void): T;
declare function f3<T>(obj: T, f1: (x: T) => void, f2: (f: (x: T) => void) => void): T;
interface Func<T> {
  (x: T): void;
}
declare function f4<T>(f1: Func<T>, f2: Func<T>): Func<T>;
declare function fo(x: Object): void;
declare function fs(x: string): void;
declare function fx(f: (x: "def") => void): void;
declare var x1: (x: string) => void;
declare var x2: "abc";
declare var x3: "def" | "abc";
declare var x4: Func<string>;
declare var never: never;
declare var x10: never;
declare var x11: "def";
declare function foo<T>(a: ReadonlyArray<T>): T;
declare var x: never;
interface A {
  a: string;
}
interface B extends A {
  b: string;
}
declare function acceptUnion(x: A | number): void;
declare function acceptA(x: A): void;
declare var a: A;
declare var b: B;
declare function coAndContra<T>(value: T, func: (t: T) => void): T;
declare var t1: A;
declare var t2: B;
declare var t3: A;
declare function coAndContraArray<T>(value: T[], func: (t: T) => void): T[];
declare var t4: A[];
declare var t5: B[];
declare var t6: A[];
