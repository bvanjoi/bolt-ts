// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/augmentedTypesModules3.ts`, Apache-2.0 License

//// module then class
namespace m3 { }
class m3 { } // ok since the module is not instantiated

namespace m3a { var y = 2; }
//~^ ERROR: A namespace declaration cannot be located prior to a class or function with which it is merged.
class m3a { foo() { } } // error, class isn't ambient or declared before the module