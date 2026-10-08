// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualSignatureInstantiation4.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail
var banana1 = fruitFactory1(Banana)// Banana<any>
;
var banana2 = fruitFactory2(Banana)// Banana<any>
;
var banana3 = fruitFactory3(Banana)// Banana<"foo">
;
var banana4 = fruitFactory4(Banana)// Banana<"foo">
;
var banana5 = fruitFactory5(Banana);