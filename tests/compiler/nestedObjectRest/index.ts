// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nestedObjectRest.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var x, y;

[{ ...x }] = [{ abc: 1 }];
for ([{ ...y }] of [[{ abc: 1 }]]) ;


enum K {
  ID = "id",
}

type Item = { [K.ID]: string };

function f({ [K.ID]: id, ...rest }: Required<Item>): Item {
  return { [K.ID]: id, ...rest };
}