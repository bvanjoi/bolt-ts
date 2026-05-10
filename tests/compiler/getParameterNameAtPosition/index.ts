// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/getParameterNameAtPosition.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

interface Mock<Y extends any[]> extends Function {
    (...args: Y): any;
}
type Tester = (opts: any, done: (...args: any[]) => any) => any;
declare function cases(tester: Tester): void;
declare function fn<Y extends any[]>(implementation?: (...args: Y) => any): Mock<Y>;
cases(fn(opts => { }));