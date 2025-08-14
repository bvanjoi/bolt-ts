// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/undefinedTypeAssignment1.ts`, Apache-2.0 License

type undefined = string; //~ ERROR: Type alias name cannot be 'undefined'.
function p(undefined = "wat") {
	return undefined;
}
