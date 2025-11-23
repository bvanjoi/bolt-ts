// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/moduleAsBaseType.ts`, Apache-2.0 License

namespace M {}
class C extends M {}
//~^ ERROR: Cannot find name 'M'.
interface I extends M { }
//~^ ERROR: Cannot find name 'M'.
class C2 implements M { }
//~^ ERROR: Cannot find name 'M'.
