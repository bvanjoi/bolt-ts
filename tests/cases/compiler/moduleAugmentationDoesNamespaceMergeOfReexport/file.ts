export namespace Root {
  export interface Foo {
      x: number;
      y: Foo;
  }
}

export const A = 1;

export declare const declaredF: () => number;