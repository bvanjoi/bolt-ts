namespace globals {
  export type Foo = {};
  export const Bar = {};
}
import Foo = globals.toString.Blah;
//~^ ERROR: Namespace 'globals' has no exported member 'toString'.