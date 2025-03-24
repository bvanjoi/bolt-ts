// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/moduleAugmentationDoesNamespaceMergeOfReexport.ts`, Apache-2.0 License

import * as ns from "./reexport";

// let a: string = ns.A;

// declare module "./reexport" {
//     export namespace Root {
//         export interface Foo {
//             self: Foo;
//         }
//     }
// }

// declare const f: ns.Root.Foo;

// f.x;
// f.self;
// f.self.x;
// f.self.self;

import * as file from './file';
let a: string = file.A;
//~^ ERROR: Type 'number' is not assignable to type 'string'.