// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/multipleClassPropertyModifiersErrors.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C {
	public public p1;
	private private p2;
	static static p3;
  //~^ ERROR: Unexpected keyword or identifier.
	public private p4;
	private public p5;
	public static p6;
	private static p7;
}