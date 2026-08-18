// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/useBeforeDeclaration_superClass.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
    x = 0;
}
class D extends C {
    // Not an error -- this will access the parent's initialized value for `x`, not the one on the child.
    old_x = this.x;
    x = 1;
}

// Test that it works on chains of classes
class X {
    x = 0;
}
class Y extends X {}
class Z extends Y {
    old_x = this.x;
    x = 1;
}

// Interface doesn't count
interface I {
    x: number;
}
class J implements I {
    old_x = this.x;
    //~^ ERROR: Property 'x' is used before its initialization.
    x = 1;
}