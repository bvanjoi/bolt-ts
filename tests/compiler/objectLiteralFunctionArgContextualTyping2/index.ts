// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLiteralFunctionArgContextualTyping2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface I2 {
    value: string;
    doStuff: (t: string) => string;
}

function f2(args: I2) { }

f2({ hello: 1 }) 
//~^ ERROR: Object literal may only specify known properties, and 'hello' does not exist in type 'I2'.
f2({ value: '' })
//~^ ERROR: Property 'doStuff' is missing.
f2({ value: '', what: 1 }) 
//~^ ERROR: Object literal may only specify known properties, and 'what' does not exist in type 'I2'.
f2({ toString: (s) => s }) 
//~^ ERROR: Type '(s: any) => any' is not assignable to type '() => string'.
f2({ toString: (s: string) => s }) 
//~^ ERROR: Type '(s: string) => string' is not assignable to type '() => string'.
f2({ value: '', toString: (s) => s.uhhh }) 
//~^ ERROR: Type '(s: any) => any' is not assignable to type '() => string'.