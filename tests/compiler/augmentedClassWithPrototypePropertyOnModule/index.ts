// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/augmentedClassWithPrototypePropertyOnModule.ts`, Apache-2.0 License

declare namespace m {
    var f;
    var prototype; // This should be error since prototype would be static property on class m
    //~^ ERROR: Duplicate identifier 'prototype'.
}
declare class m {
}