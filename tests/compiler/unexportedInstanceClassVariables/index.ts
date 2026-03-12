// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/unexportedInstanceClassVariables.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace M{
	class A{
		constructor(val:string){}
	}
}

namespace M{
	class A {}  
 
 	var a = new A();
}