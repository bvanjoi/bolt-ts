// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/protectedAccessThroughContextualThis.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

class Foo {
  protected protec = 'bar';
  private privat = '';
  copy!: string
  constructor() {
    bindCopy.call(this)
    bindCopy2.call(this)
  }
}

function bindCopy(this: Foo) {
  this.copy = this.protec; // Should OK
  console.log(this.privat); // Should error
  //~^ ERROR: Property 'privat' is private and only accessible within class.
}

type BindingFunction = (this: Foo) => void;

const bindCopy2: BindingFunction = function () {
  this.copy = this.protec; // Should OK
  console.log(this.privat); // Should error
  //~^ ERROR: Property 'privat' is private and only accessible within class.
}