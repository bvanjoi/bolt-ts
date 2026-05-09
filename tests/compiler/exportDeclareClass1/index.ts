// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportDeclareClass1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: module=commonjs

    export declare class eaC {
        static tF() { };
        //~^ ERROR: An implementation cannot be declared in ambient contexts.
        static tsF(param:any) { };
        //~^ ERROR: An implementation cannot be declared in ambient contexts.
    };
	
	export declare class eaC2 {
        static tF();
        static tsF(param:any);
    };
