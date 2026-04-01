// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/typeParameterExplicitlyExtendsAny.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function fee<T>() {
    var t!: T;
    t.blah; // Error
    //~^ ERROR: Property 'blah' does not exist on type 'T'.
    t.toString; // ok
}

function fee2<T extends any>() {
    var t!: T;
    t.blah; // ok
    //~^ ERROR: Property 'blah' does not exist on type 'T'.
    t.toString; // ok
}

function f<T extends any>(x: T) {
    x.children;
    //~^ ERROR: Property 'children' does not exist on type 'T'.
    x();
    //~^ ERROR: This expression is not callable.
    new x();
    //~^ ERROR: This expression is not constructable.
    x[100];
    x['hello'];
}


// Generic Tree structure
type Tree<T> = T & {
    children?: Tree<T>[];
}

class MyClass {
    public static displayTree1<T extends Tree<any>>(tree: T) {
        // error "Property 'children' does not exist on type 'T'"
        tree.children;
        //~^ ERROR: Property 'children' does not exist on type 'T'.
    }
}
