import * as _ from "./a";

for (const _ of []) { }

for (const _ in []) { }

namespace _ns {
  //~^ ERROR: '_ns' is declared but its value is never read.
    let _;
    //~^ ERROR: '_' is declared but its value is never read.
    for (const _ of []) { }

    for (const _ in []) { }
}