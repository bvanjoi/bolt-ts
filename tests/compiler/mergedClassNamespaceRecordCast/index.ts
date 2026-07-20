// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mergedClassNamespaceRecordCast.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C1 { foo() {} }

new C1() as Record<string, unknown>;
//~^ ERROR: Conversion of type 'C1' to type 'Record<string, unknown>' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.

class C2 { foo() {} }
namespace C2 { export const unrelated = 3; }

new C2() as Record<string, unknown>;
//~^ ERROR: Conversion of type 'C2' to type 'Record<string, unknown>' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.

C2.unrelated
new C2().unrelated
//~^ ERROR: Property 'unrelated' does not exist on type 'C2'.


namespace C3 { export const unrelated = 3; }

C3 as Record<string, unknown>;
