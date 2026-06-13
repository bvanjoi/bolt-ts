declare function f1<T>(x: T): void;
declare function error(): never;
declare function f2<T>(x: T): NonNullable<T>;
declare function f3(x: unknown): void;
declare function f4<T extends {
  x: string;
} | undefined>(obj: T): void;
declare class A {
  x;
  foo(): void;
}
