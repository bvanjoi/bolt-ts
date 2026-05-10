// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/overloadResolutionWithAny.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

var func: {
    (s: string): number;
    (s: any): string;
};

func(""); // number
func(3); // string
var x: any;
func(x); // string

var func2: {
    (s: string, t: string): number;
    (s: any, t: string): boolean;
    (s: string, t: any): RegExp;
    (s: any, t: any): string;
}

func2(x, x); // string
func2("", ""); // number
func2(x, ""); // boolean
func2("", x); // RegExp