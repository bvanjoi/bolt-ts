// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classMemberInitializerScoping2.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[a]     compiler-options: target=es2017
//@[a]     compiler-options: useDefineForClassFields
//@[b]     compiler-options: target=es2017
//@[b]     compiler-options: useDefineForClassFields=false
//@[c]     compiler-options: target=esnext
//@[c]     compiler-options: useDefineForClassFields
//@[d]     compiler-options: target=esnext
//@[d]     compiler-options: useDefineForClassFields=false

const x = 1
class C {
    p = x
    //~[a]^ ERROR: Initializer of instance member variable 'p' cannot reference identifier 'x' declared in the constructor.
    //~[b]^^ ERROR: Initializer of instance member variable 'p' cannot reference identifier 'x' declared in the constructor.
    //~[d]^^^ ERROR: Initializer of instance member variable 'p' cannot reference identifier 'x' declared in the constructor.
    constructor(x: string) { }
}
