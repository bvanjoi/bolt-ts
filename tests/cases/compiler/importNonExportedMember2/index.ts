// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/importNonExportedMember2.ts`, Apache-2.0 License

import { Foo } from './a';
//~^ ERROR: Module '"./a"' declares 'Foo' locally, but it is not exported.

import { Bar } from './a';
