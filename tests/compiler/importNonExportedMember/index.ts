// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/importNonExportedMember.ts`, Apache-2.0 License

import { foo, bar } from "./a";
//~^ ERROR: Module '"./a"' declares 'bar' locally, but it is exported as 'baz'.

foo(1);
//~^ ERROR: Expected 0 arguments, but got 1.