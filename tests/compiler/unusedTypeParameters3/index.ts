// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedTypeParameters3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

class greeter<typeparameter1, typeparameter2, typeparameter3> {
  //~^ ERROR: 'typeparameter1' is declared but its value is never read.
  //~| ERROR: 'typeparameter3' is declared but its value is never read.
    private x: typeparameter2;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.

    public function1() {
        this.x;
    }
}