// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recur1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

var salt:any = new salt.pepper();   
salt.pepper = function() {}

var cobalt = new cobalt.pitch();   
cobalt.pitch = function() {}
 
