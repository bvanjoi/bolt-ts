// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es5ExportDefaultClassDeclaration3.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: declaration

var before: C = new C();
//~^ ERROR: Class 'C' used before its declaration.

export default class C {
    method(): C {
        return new C();
    }
}

var after: C = new C();

var t: typeof C = C;

