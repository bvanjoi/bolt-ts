// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/partiallyDiscriminantedUnions.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A1 {
    type: 'a';
    subtype: 1;
}

interface A2 {
    type: 'a';
    subtype: 2;
    foo: number;
}

interface B {
    type: 'b';
}

type AB = A1 | A2 | B;

const ab: AB = <AB>{};

if (ab.type === 'a') {
    if (ab.subtype === 2) {
        ab.foo;
    }
}

// Repro from #11185

class Square { kind: "square"; }
//~^ ERROR: Property 'kind' has no initializer and is not definitely assigned in the constructor.
class Circle { kind: "circle"; }
//~^ ERROR: Property 'kind' has no initializer and is not definitely assigned in the constructor.

type Shape = Circle | Square;
type Shapes = Shape | Array<Shape>;

function isShape(s : Shapes): s is Shape {
    return !Array.isArray(s);
}

function fail(s: Shapes) {
    if (isShape(s)) {
        if (s.kind === "circle") {
            let c: Circle = s;
        }
    }
}