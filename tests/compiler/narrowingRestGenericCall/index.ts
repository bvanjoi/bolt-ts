// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowingRestGenericCall.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

interface Slugs {
  foo: string;
  bar: string;
}

function call<T extends object>(obj: T, cb: (val: T) => void) {
  cb(obj);
}

declare let obj: Slugs;
call(obj, ({foo, ...rest}) => {
  console.log(rest.bar);
});