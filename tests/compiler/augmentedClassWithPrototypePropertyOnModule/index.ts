// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/augmentedClassWithPrototypePropertyOnModule.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare namespace m {
    var f;
    var prototype; // This should be error since prototype would be static property on class m
    //~^ ERROR: Duplicate identifier 'prototype'.
}
declare class m {
}
