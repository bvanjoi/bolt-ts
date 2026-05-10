// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLiteralFunctionArgContextualTyping.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I {
    value: string;
    toString: (t: string) => string;
}

function f2(args: I) { }

f2({ hello: 1 }) // error 
//~^ ERROR: Object literal may only specify known properties, and 'hello' does not exist in type 'I'.
f2({ value: '' }) // missing toString satisfied by Object's member
f2({ value: '', what: 1 }) // missing toString satisfied by Object's member
//~^ ERROR: Object literal may only specify known properties, and 'what' does not exist in type 'I'.
f2({ toString: (s) => s }) // error, missing property value from ArgsString
//~^ ERROR: Property 'value' is missing.
f2({ toString: (s: string) => s }) // error, missing property value from ArgsString
//~^ ERROR: Property 'value' is missing.
f2({ value: '', toString: (s) => s.uhhh }) // error
//~^ ERROR: Property 'uhhh' does not exist on type 'string'.