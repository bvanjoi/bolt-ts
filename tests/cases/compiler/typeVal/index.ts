// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeVal.ts`, Apache-2.0 License

interface I {
  I:number;
}

var I:I = { I: 3};
I.I=4;

