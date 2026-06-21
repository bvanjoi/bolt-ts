// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inKeywordAndIntersection.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

class A { a = 0 }
class B { b = 0 }

function f10(obj: A & { x: string } | B) {
    if (obj instanceof Object) {
        obj;  // A & { x: string } | B
    }
    else {
        obj;  // Error
    }
}

// Repro from #50844

interface InstanceOne {
    one(): void
}

interface InstanceTwo {
    two(): void
}

const instance = {} as InstanceOne | InstanceTwo

const ClassOne = {} as { new(): InstanceOne } & { foo: true };

if (instance instanceof ClassOne) {
    instance.one();
}
