type A = {
}

// @ts-expect-error
const n: number = '42';

namespace P {
  export interface T {
    name?: string
  }
}
type P = P.T;
{
  function f(p: P) {
    const _: string | undefined = p.name;
  }
}