// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/deleteReadonly.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface A {
  readonly b
}
var a: A = {
  b: 123
};

delete a.b;
//~^ ERROR: The operand of a 'delete' operator cannot be a read-only property.

interface B {
  readonly [k: string]: string
}

var b: B = {
  'test': 'test'
};

delete b['test'];
//~^ ERROR: Index signature in type 'B' only permits reading.

delete ((((b['test']))));
//~^ ERROR: Index signature in type 'B' only permits reading.
