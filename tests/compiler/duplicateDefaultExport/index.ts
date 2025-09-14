// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/duplicateDefaultExport.ts`, Apache-2.0 License

export default 0;
export default function() {}
//~^ ERROR: A module cannot have multiple default exports.
