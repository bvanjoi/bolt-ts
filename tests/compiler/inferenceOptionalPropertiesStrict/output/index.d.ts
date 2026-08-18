declare function test<T>(x: {
  [key:string]: T;
}): T;
declare var x1: {
  a?: string;
  b?: number;
};
declare var x2: {
  a?: string;
  b?: number | undefined;
};
declare var y1: number | string;
declare var y2: undefined | number | string;
declare var v1: Required<{
  a?: string;
  b?: number;
}>;
declare var v1: {
  a: string;
  b: number;
};
declare var v2: Required<{
  a?: string;
  b?: number | undefined;
}>;
declare var v2: {
  a: string;
  b: number | undefined;
};
declare var v3: Partial<{
  a: string;
  b: string;
}>;
declare var v3: {
  a?: string;
  b?: string;
};
declare var v4: Partial<{
  a: string;
  b: string | undefined;
}>;
declare var v4: {
  a?: string;
  b?: string | undefined;
};
declare var v5: Required<Partial<{
  a: string;
  b: string;
}>>;
declare var v5: {
  a: string;
  b: string;
};
declare var v6: Required<Partial<{
  a: string;
  b: string | undefined;
}>>;
declare var v6: {
  a: string;
  b: string | undefined;
};
