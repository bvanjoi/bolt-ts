// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es5SetterparameterDestructuringNotElided.ts`, Apache-2.0 License
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
var foo = {
  set foo([start, end]) {
    void start;
    void end;
  }  
};