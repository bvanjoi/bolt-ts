// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/badThisBinding.ts`, Apache-2.0 License

declare function foo(a:any): any;
declare function bar(a:any): any;

class Greeter {
    constructor() {
		foo(() => {
            bar(() => {
                var x = this;
			});
		});
	}

} 