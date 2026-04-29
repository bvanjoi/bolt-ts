// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentStricterConstraints.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var f = function <T, S extends T>(x: T, y: S): void {
    x = y
}

var g = function <T, S>(x: T, y: S): void { }

g = f
//~^ ERROR: Type '(x: T, y: S) => void' is not assignable to type '(x: T, y: S) => void'.
g(1, "")
