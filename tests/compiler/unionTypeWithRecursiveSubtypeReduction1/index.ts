// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/unionTypeWithRecursiveSubtypeReduction1.ts`, Apache-2.0 License

//@ run-fail

class Module {
    public members: Class[];
    //~^ ERROR: Property 'members' has no initializer and is not definitely assigned in the constructor.
}

class Namespace {
    public members: (Class | Property)[];
    //~^ ERROR: Property 'members' has no initializer and is not definitely assigned in the constructor.
}

class Class {
    public parent: Namespace;
    //~^ ERROR: Property 'parent' has no initializer and is not definitely assigned in the constructor.
}

class Property {
    public parent: Module | Class;
    //~^ ERROR: Property 'parent' has no initializer and is not definitely assigned in the constructor.
}

var t: Class | Property;
t.parent;
//~^ ERROR: Variable 't' is used before being assigned.
