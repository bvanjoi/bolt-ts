declare function f1(...args: any): void;
declare function f2(x: (...args: any) => void): void;
declare function f3(x: {
  (...args: any): void;
}): void;
declare function f4<T extends (...args: any) => void>(): void;
declare function f5<T extends {
  (...args: any): void;
}>(): void;
declare var f6: () => any[];

