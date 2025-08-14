// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/nonContextuallyTypedLogicalOr.ts`, Apache-2.0 License

//@ run-fail

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