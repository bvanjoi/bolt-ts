// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/selfInCallback.ts`, Apache-2.0 License

class C {
	public p1 = 0;
	public callback(cb:()=>void) {cb();}
	public doit() {
		this.callback(()=>{this.p1+1});
	}
}