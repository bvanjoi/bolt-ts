// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unionTypeWithRecursiveSubtypeReduction2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

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

declare var c: Class;
declare var p: Property;
c = p;
//~^ ERROR: Property 'members' is missing.
p = c;
//~^ ERROR: Type 'Class' is not assignable to type 'Property'.