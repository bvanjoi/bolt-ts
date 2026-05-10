// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/objectInstantiationFromUnionSpread.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictFunctionTypes

interface Success {
  isSuccess: true;
}

interface Fail {
  isSuccess: false;
}

type Item = Success | Fail;

function f1(a: Item[]) {
  a.map(item => ({ ...item })).filter(value => {});
}

function f2<T>(a: Item[]) {
  a.map(item => ({ ...item })).filter(value => {});
}