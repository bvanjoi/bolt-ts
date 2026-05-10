// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeParameterLeak.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

interface Box<T> { data: T }
type BoxTypes = Box<{ x: string }> | Box<{ y: string }>;

type BoxFactoryFactory<TBox> = TBox extends Box<infer T> ? {
  (arg: T): BoxFactory<TBox> | undefined
} : never;

interface BoxFactory<A> {
  getBox(): A,
}

declare const f: BoxFactoryFactory<BoxTypes>;
const b = f({ x: "", y: "" })?.getBox();
if (b) {
  const x = b.data;
}
