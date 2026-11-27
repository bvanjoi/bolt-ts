import defaultBinding, * as nameSpaceBinding  from "./es6ImportDefaultBindingFollowedWithNamespaceBindingInEs5_0";
//~^ ERROR: Module './es6ImportDefaultBindingFollowedWithNamespaceBindingInEs5_0' has no default export.
var x: number = nameSpaceBinding.a;