// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/newArrays.ts`, Apache-2.0 License

module M {
	class Foo {}
	class Gar {
		public fa: Foo[];
		public x = 10;
		public y = 10;

		public m () {
			this.fa = new Array<Foo>(this.x * this.y);		
		}
	}
}