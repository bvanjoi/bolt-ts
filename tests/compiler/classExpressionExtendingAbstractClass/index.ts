// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/classExpressionExtendingAbstractClass.ts`, Apache-2.0 License

abstract class A {
    abstract foo(): void;
}

var C = class extends A {     // no error reported!
//~^ ERROR: Non-abstract class expression does not implement inherited abstract member 'foo' from class 'A'.
};
