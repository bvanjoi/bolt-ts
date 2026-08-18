// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reachabilityCheckWithEmptyDefault.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare function print(s: string): void;
function foo(x: any) {
	switch(x) {
		case 1: return;
		default:
	}
	print('1');
}