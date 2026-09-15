// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeofThisInMethodSignature.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

export class A {
	x = 1
	a(x: typeof this.x): void {}
}

const a = new A().a(1);
