// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/unionTypeWithRecursiveSubtypeReduction1.ts`, Apache-2.0 License

//@ run-fail

class Module {
    public members: Class[];
}

class Namespace {
    public members: (Class | Property)[];
}

class Class {
    public parent: Namespace;
}

class Property {
    public parent: Module | Class;
}

var t: Class | Property;
t.parent;
