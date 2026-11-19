import {N} from "./f1";
//~^ ERROR: Import declaration conflicts with local declaration of 'N'.
export namespace N {
    export interface I {x: any}
}