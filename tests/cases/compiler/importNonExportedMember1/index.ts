// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/importNonExportedMember1.ts`, Apache-2.0 License

import { bar } from "./a";
//~^ ERROR: Module '"./a"' declares 'bar' locally, but it is not exported.

