// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parseInvalidNullableTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@skip-message-match

function f1(a: string): a is ?string {
    return true;
}

function f2(a: string?) {}
function f3(a: number?) {}

function f4(a: ?string) {}
function f5(a: ?number) {}

function f6(a: string): ?string {
    return true;
}

const a = 1 as any?;
const b: number? = 1;

const c = 1 as ?any;
const d: ?number = 1;

let e: unknown?;
let f: never?;
let g: void?;
let h: undefined?;
