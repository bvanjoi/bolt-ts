// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/invalidThisEmitInContextualObjectLiteral.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface IDef {
	p1: (e:string) => void;
	p2: () => (n: number) => any;
}

class TestController {
	public m(def: IDef) { }
	public p = this.m({
		p1: e => { },
		p2: () => { return vvvvvvvvv => this; },
	});
}
