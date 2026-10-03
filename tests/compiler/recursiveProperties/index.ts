// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveProperties.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=ES5
//@[target=ES2015]  compiler-options: target=ES2015

class A {
    get testProp() { return this.testProp; }
}

class B {
    set testProp(value:string) { this.testProp = value; }
}