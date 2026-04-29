// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentNestedInLiterals.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

var target, x, y;
target = [x = 1, y = x];

var aegis, a, b;
aegis = { x: a = 1, y: b = a };

var kowloona, c, d;
for (kowloona of [c = 1, d = c]) {
}
