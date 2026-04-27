// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/objectLitTargetTypeCallSite.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function process( x: {a:number; b:string;}) {
	return x.a;
}

process({a:true,b:"y"});
//~^ ERROR: Type 'true' is not assignable to type 'number'.