// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/newAbstractInstance2.ts`, Apache-2.0 License

import A from "./a";
new A();
//~^ ERROR: Cannot create an instance of an abstract class.