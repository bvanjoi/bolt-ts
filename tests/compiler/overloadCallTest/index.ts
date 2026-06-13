// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadCallTest.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class foo {
    constructor() {
        function bar(): string;
        function bar(s:string);
        function bar(foo?: string) { return "foo" };

        var test = bar("test");
        var goo = bar();

        goo = bar("test");
    }
 
}