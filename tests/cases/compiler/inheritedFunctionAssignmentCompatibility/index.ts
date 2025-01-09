// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/inheritedFunctionAssignmentCompatibility.ts`, Apache-2.0 License

interface IResultCallback extends Function { }

function fn(cb: IResultCallback) { }

fn((a, b) => true);
fn(function (a, b) { return true; })

