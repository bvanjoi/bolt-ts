// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declareDottedExtend.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

declare namespace A.B
{
    export class C{ }
}

import ab = A.B;

class D extends ab.C{ }

class E extends A.B.C{ }
