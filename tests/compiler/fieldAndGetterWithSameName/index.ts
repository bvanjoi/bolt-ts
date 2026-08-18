// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/fieldAndGetterWithSameName.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export class C {
    x: number;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
  get x(): number { return 1; }
    //~^ ERROR: Duplicate identifier 'x'.
    //~| ERROR: Duplicate identifier 'x'.
}
