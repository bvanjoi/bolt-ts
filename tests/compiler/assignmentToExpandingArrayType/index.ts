// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentToExpandingArrayType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

let x = []
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' } // previously ran out of memory here
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
x[0] = { foo: 'hi' }
