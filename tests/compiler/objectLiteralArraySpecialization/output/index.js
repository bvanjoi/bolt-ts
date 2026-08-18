// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/objectLiteralArraySpecialization.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
var thing = create([{
  name: 'bob',
  id: 24  
}, {
  name: 'doug',
  id: 32  
}]);
thing.doSomething((x, y) => (x.name === 'bob'));