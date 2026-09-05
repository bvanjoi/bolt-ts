// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/module_augmentExistingAmbientVariable.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es5]

declare var console: any;

namespace console {
  //~^ ERROR: Duplicate identifier 'console'.
    export var x = 2;
}
