// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/unionTypeErrorMessageTypeRefs01.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Foo { foo: any }
interface Bar { bar: any }
interface Baz { baz: any }
interface Kwah { kwah: any }

////////

interface A<T> {
    aProp: T;
}

interface B<T> {
    bProp: T;
}

interface C<T> {
    cProp: T;
}

declare const a: A<Foo>;
declare const b: B<Foo>;
declare const c: C<Foo>;
declare let thingOfInterfaces: A<Bar> | B<Baz> | C<Kwah>;

thingOfInterfaces = a;
//~^ ERROR: Type 'A<Foo>' is not assignable to type 'A<Bar> | B<Baz> | C<Kwah>'.
thingOfInterfaces = b;
//~^ ERROR: Type 'B<Foo>' is not assignable to type 'A<Bar> | B<Baz> | C<Kwah>'.
thingOfInterfaces = c;
//~^ ERROR: Type 'C<Foo>' is not assignable to type 'A<Bar> | B<Baz> | C<Kwah>'.

////////

type X<T> = {
    xProp: T;
}

type Y<T> = {
    yProp: T;
}

type Z<T> = {
    zProp: T;
}

declare const x: X<Foo>;
declare const y: Y<Foo>;
declare const z: Z<Foo>;
declare let thingOfTypeAliases: X<Bar> | Y<Baz> | Z<Kwah>;

thingOfTypeAliases = x;
//~^ ERROR: Type 'X' is not assignable to type 'X | Y | Z'.
thingOfTypeAliases = y;
//~^ ERROR: Type 'Y' is not assignable to type 'X | Y | Z'.
thingOfTypeAliases = z;
//~^ ERROR: Type 'Z' is not assignable to type 'X | Y | Z'.