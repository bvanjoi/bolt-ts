// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitDestructuring4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

// For an array binding pattern with empty elements,
// we will not make any modification and will emit
// the similar binding pattern users' have written
function baz([]) { }
function baz1([] = [1,2,3]) { }
function baz2([[]] = [[1,2,3]]) { }

function baz3({}) { }
function baz4({} = { x: 10 }) { }