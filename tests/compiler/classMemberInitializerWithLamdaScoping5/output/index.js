
// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classMemberInitializerWithLamdaScoping5.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: lib=[es5]
class Greeter {
  constructor(message) {}
  messageHandler = (message) => {
    console.log(message);// This shouldnt be error
    
  };
}