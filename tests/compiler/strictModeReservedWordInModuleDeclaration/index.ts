// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/strictModeReservedWordInModuleDeclaration.ts`, Apache-2.0 License

//@compiler-options: target=es2015

"use strict"
namespace public { }
//~^ ERROR: Identifier expected. 'public' is a reserved word in strict mode.
namespace private { }
//~^ ERROR: Identifier expected. 'private' is a reserved word in strict mode.
namespace public.whatever {
//~^ ERROR: Identifier expected. 'public' is a reserved word in strict mode.
}
namespace private.public.foo { }
//~^ ERROR: Identifier expected. 'private' is a reserved word in strict mode.
//~| ERROR: Identifier expected. 'public' is a reserved word in strict mode.

declare namespace protected { }
