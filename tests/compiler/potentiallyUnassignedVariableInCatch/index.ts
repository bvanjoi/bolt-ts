// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/potentiallyUnassignedVariableInCatch.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

let foo;
try {
	if (Math.random() > 0.5) {
		foo = 1234;
	}
} catch {
	foo;
}
