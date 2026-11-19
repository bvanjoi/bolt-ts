import defaultBinding1, { } from "./es6ImportDefaultBindingFollowedWithNamedImport1InEs5_0";
var x: number = defaultBinding1;
import defaultBinding2, { a } from "./es6ImportDefaultBindingFollowedWithNamedImport1InEs5_0";
//~^ ERROR: Module '"./es6ImportDefaultBindingFollowedWithNamedImport1InEs5_0"' has no exported member 'a'. Did you mean to use 'import a from "./es6ImportDefaultBindingFollowedWithNamedImport1InEs5_0"' instead?
var x: number = defaultBinding2;
import defaultBinding3, { a as b } from "./es6ImportDefaultBindingFollowedWithNamedImport1InEs5_0";
//~^ ERROR: Module '"./es6ImportDefaultBindingFollowedWithNamedImport1InEs5_0"' has no exported member 'a'. Did you mean to use 'import a from "./es6ImportDefaultBindingFollowedWithNamedImport1InEs5_0"' instead?
var x: number = defaultBinding3;
import defaultBinding4, { x, a as y } from "./es6ImportDefaultBindingFollowedWithNamedImport1InEs5_0";
//~^ ERROR: Module '"./es6ImportDefaultBindingFollowedWithNamedImport1InEs5_0"' has no exported member 'a'. Did you mean to use 'import a from "./es6ImportDefaultBindingFollowedWithNamedImport1InEs5_0"' instead?
//~| ERROR: Module '"./es6ImportDefaultBindingFollowedWithNamedImport1InEs5_0"' has no exported member 'x'. Did you mean to use 'import x from "./es6ImportDefaultBindingFollowedWithNamedImport1InEs5_0"' instead?
var x: number = defaultBinding4;
import defaultBinding5, { x as z,  } from "./es6ImportDefaultBindingFollowedWithNamedImport1InEs5_0";
//~^ ERROR: Module '"./es6ImportDefaultBindingFollowedWithNamedImport1InEs5_0"' has no exported member 'x'. Did you mean to use 'import x from "./es6ImportDefaultBindingFollowedWithNamedImport1InEs5_0"' instead?
var x: number = defaultBinding5;
import defaultBinding6, { m,  } from "./es6ImportDefaultBindingFollowedWithNamedImport1InEs5_0";
//~^ ERROR: Module '"./es6ImportDefaultBindingFollowedWithNamedImport1InEs5_0"' has no exported member 'm'. Did you mean to use 'import m from "./es6ImportDefaultBindingFollowedWithNamedImport1InEs5_0"' instead?
var x: number = defaultBinding6;