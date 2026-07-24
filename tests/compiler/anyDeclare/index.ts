// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/anyDeclare.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare var x: any;
namespace myMod {
    var myFn;
    function myFn() {  }
    //~^ ERROR: Duplicate identifier 'myFn'.
}