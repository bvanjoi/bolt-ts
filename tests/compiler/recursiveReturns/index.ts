// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveReturns.ts`, Apache-2.0 License

function R1() {
	R1();
	return;
}

function R2() { R2(); }

function R3(n:number) {
	if (n == 0) {
		//return;
	}
	else {
		R3(n--);
	}
}