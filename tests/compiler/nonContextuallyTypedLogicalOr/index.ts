// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nonContextuallyTypedLogicalOr.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

interface Contextual {
  dummy;
  p?: number;
}

interface Ellement {
  dummy;
  p: any;
}

var c: Contextual;
var e: Ellement;

(c || e).dummy;
