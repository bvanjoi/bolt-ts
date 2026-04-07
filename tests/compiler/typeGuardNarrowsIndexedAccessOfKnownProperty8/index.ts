// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeGuardNarrowsIndexedAccessOfKnownProperty8.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: target=esnext

import * as a from "./a";
export class C {
    [a.key]: string;

    constructor() {
        this[a.key] = "foo";
    }
}

const a0: 'b' = a.key;
//~^ ERROR: Type '"a"' is not assignable to type '"b"'.


export {type default as tag} from './b';
