// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/complexClassRelationships.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

class Derived extends Base {
  //~^ ERROR: Class 'Base' used before its declaration.
  //~| ERROR: Class 'Base' used before its declaration.
    public static createEmpty(): Derived {
        var item = new Derived();
        return item;
    }
}
class BaseCollection<T extends Base> {
    constructor(f: () => T) {
        (item: Thing) => { return [item.Components]; };
    }
}
class Base {
    ownerCollection: BaseCollection<Base>;
    //~^ ERROR: Property 'ownerCollection' has no initializer and is not definitely assigned in the constructor.
}

class Thing {
    public get Components(): ComponentCollection<any> { return null }
    //~^ ERROR: Type 'null' is not assignable to type 'ComponentCollection<any>'.
}

class ComponentCollection<T> {
    private static sortComponents(p: Foo) {
        return p.prop1;
    }
}

class Foo {
    public get prop1() {
        return new GenericType<string>(this);
    }
    public populate() {
        this.prop2;
    }
    public get prop2(): BaseCollection<Derived> {
        return new BaseCollection<Derived>(Derived.createEmpty);
    }
}

class GenericType<T> {
    constructor(parent: FooBase) { }
}

class FooBase {
    public populate() {

    }
}