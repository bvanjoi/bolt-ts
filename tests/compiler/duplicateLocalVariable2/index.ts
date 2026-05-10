// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/duplicateLocalVariable2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=esnext

export class TestCase {
    constructor (public name: string, public test: ()=>boolean, public errorMessageRegEx?: string) {
    }
}
export class TestRunner { 
    static arrayCompare(arg1: any[], arg2: any[]): boolean {
        return false;
    }

    public addTest(test: TestCase) {
    }
}

export var tests: TestRunner = (function () {
    var testRunner = new TestRunner();

    testRunner.addTest(new TestCase("Check UTF8 encoding",
        function () {
            var fb: any;
            fb.writeUtf8Bom();
            var chars = [0x0054];
            for (var i in chars) {
                fb.writeUtf8CodePoint(chars[i]);
            }
            fb.index = 0;
            var bytes = [];
            for (var i = 0; i < 14; i++) {
              //~^ ERROR: Subsequent variable declarations must have the same type. Variable 'i' must be of type 'string', but here has type 'number'.
              //~| ERROR: Operator '<' cannot be applied to types 'string' and 'number'.
              //~| ERROR: An arithmetic operand must be of type 'any', 'number', 'bigint' or an enum type.
                bytes.push(fb.readByte());
            }
            var expected = [0xEF];
            return TestRunner.arrayCompare(bytes, expected);
        }));

    return testRunner;
})();