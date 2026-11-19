// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/abstractPropertyNegative.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

interface A {
    prop: string;
    m(): string;
}
abstract class B implements A {
    abstract prop: string;
    public abstract readonly ro: string;
    abstract get readonlyProp(): string;
    abstract m(): string;
    abstract get mismatch(): string;
    abstract set mismatch(val: number);
}
class C extends B {
  //~^ ERROR: Non-abstract class 'C' does not implement inherited abstract member 'mismatch' from class 'B'.
    readonly ro = "readonly please";
    abstract notAllowed: string;
    //~^ ERROR: Abstract modifier can only appear within an abstract class.
    get concreteWithNoBody(): string;
}
//~^ ERROR: Expected '{'.
let c = new C();
c.ro = "error: lhs of assignment can't be readonly";
//~^ ERROR: Cannot assign to 'ro' because it is a read-only property.

abstract class WrongTypeProperty {
    abstract num: number;
}
class WrongTypePropertyImpl extends WrongTypeProperty {
    num = "nope, wrong";
    //~^ ERROR: Property 'num' in type 'WrongTypePropertyImpl<WrongTypePropertyImpl>' is not assignable to the same property in base type 'WrongTypeProperty<WrongTypePropertyImpl>'.
}
abstract class WrongTypeAccessor {
    abstract get num(): number;
}
class WrongTypeAccessorImpl extends WrongTypeAccessor {
    get num() { return "nope, wrong"; }
    //~^ ERROR: Property 'num' in type 'WrongTypeAccessorImpl<WrongTypeAccessorImpl>' is not assignable to the same property in base type 'WrongTypeAccessor<WrongTypeAccessorImpl>'.
}
class WrongTypeAccessorImpl2 extends WrongTypeAccessor {
    num = "nope, wrong";
    //~^ ERROR: Property 'num' in type 'WrongTypeAccessorImpl2<WrongTypeAccessorImpl2>' is not assignable to the same property in base type 'WrongTypeAccessor<WrongTypeAccessorImpl2>'.
}

abstract class AbstractAccessorMismatch {
    abstract get p1(): string;
    //~^ ERROR: Accessors must both be abstract or non-abstract.
    set p1(val: string) { };
    get p2(): string { return "should work"; }
    //~^ ERROR: Accessors must both be abstract or non-abstract.
    abstract set p2(val: string);
}
