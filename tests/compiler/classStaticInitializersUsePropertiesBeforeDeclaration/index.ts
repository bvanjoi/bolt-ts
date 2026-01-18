// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classStaticInitializersUsePropertiesBeforeDeclaration.ts`, Apache-2.0 License

class Foo {
    static enumMember = Enum.A;
    //~^ ERROR: Enum 'Enum' used before its declaration.
    //~| ERROR: Property 'A' is used before its initialization.
    static objLiteralMember = ObjLiteral.A;
    //~^ ERROR: Block-scoped variable 'ObjLiteral' used before its declaration.
    //~| ERROR: Property 'A' is used before its initialization.
    static namespaceMember = Namespace.A;
    //~^ ERROR: Property 'A' is used before its initialization.
}

enum Enum {
    A
}

const ObjLiteral = {
    A: 0
};

namespace Namespace {
    export let A = 0
}
