// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/deeplyNestedCheck.ts`, Apache-2.0 License

interface DataSnapshot<X = {}> {
  child(path: string): DataSnapshot;
}

interface Snapshot<T> extends DataSnapshot {
  child<U extends Extract<keyof T, string>>(path: U): Snapshot<T[U]>;
}

interface A { b: B[] }
interface B { c: C }
interface C { d: D[] }
interface D { e: E[] }
interface E { f: F[] }
interface F { g: G }
interface G { h: H[] }
interface H { i: string }

const x: A = {
  b: [
    {
      c: {
        d: [
          {
            e: [
              {
                f: [
                  {
                    g: {
                      h: [
                        { //~ ERROR Property 'i' is missing.
                          // i: '',
                        },
                      ],
                    },
                  },
                ],
              },
            ],
          },
        ],
      },
    },
  ],
};


const a1: string[][][][][] = [[[[[42]]]]];
//~^ ERROR: Type 'number' is not assignable to type 'string'.
const a2: string[][][][][][][][][][] = [[[[[[[[[[42]]]]]]]]]];
//~^ ERROR: Type 'number' is not assignable to type 'string'.
