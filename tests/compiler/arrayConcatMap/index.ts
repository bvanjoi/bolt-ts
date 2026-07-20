// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/arrayConcatMap.ts`, Apache-2.0 License

var x = [].concat([{ a: 1 }], [{ a: 2 }])
          .map(b => b.a);
//~^^ ERROR: No overload matches this call.
          //~^^ ERROR: Property 'a' does not exist on type 'T'.
