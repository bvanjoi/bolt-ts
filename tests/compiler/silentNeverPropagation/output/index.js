// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/silentNeverPropagation.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration
//@run-fail
var breaks = convert(createModule({
  a: 12  
}, {
  foo() {
    return true;
  }  
}));
breaks.state.a;
breaks.state.z;
breaks.foo();