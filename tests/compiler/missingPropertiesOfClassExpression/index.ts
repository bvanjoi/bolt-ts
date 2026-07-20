// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/missingPropertiesOfClassExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class George extends class { reset() { return this.y; } } {
  //~^ ERROR: Property 'y' does not exist on type '(Anonymous class)<(Anonymous class)>'.
    constructor() {
        super();
    }
}
