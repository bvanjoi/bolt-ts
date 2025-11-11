// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/inheritedModuleMembersForClodule.ts`, Apache-2.0 License

class C {
    static foo(): string {
        return "123";
    }
}

class D extends C {
//~^ ERROR: Class static side 'typeof D' incorrectly extends base class static side 'typeof C'.
}

namespace D {
    export function foo(): number {
        return 0;
    };
}

class E extends D {
    static bar() {
        return this.foo();
    }
}
