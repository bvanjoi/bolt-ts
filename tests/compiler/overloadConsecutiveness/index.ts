// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadConsecutiveness.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@skip-message-match

function f1(), function f1();
function f2(), function f2() {}
function f3() {}, function f3();

class C {
	m1(), m1();
	m2(), m2() {}
	m3() {}, m3();
}
