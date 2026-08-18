// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceClassMerging2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Foo {
    interfaceFooMethod(): this;
    interfaceFooProperty: this;
}

class Foo {
    classFooProperty: this;
    //~^ ERROR: Property 'classFooProperty' has no initializer and is not definitely assigned in the constructor.

    classFooMethod(): this {
        return this;
    }
}


interface Bar {
    interfaceBarMethod(): this;
    interfaceBarProperty: this;
}

class Bar extends Foo {
    classBarProperty: this;
    //~^ ERROR: Property 'classBarProperty' has no initializer and is not definitely assigned in the constructor.

    classBarMethod(): this {
        return this;
    }
}


var bar = new Bar();
bar.interfaceBarMethod().interfaceFooMethod().classBarMethod().classFooMethod();


var foo = new Foo();

foo = bar;