import B = require("./unusedImportDeclaration_testerB");
var thingy: B = {
    me: "A"
};
declare function foo(a: string): void;
foo("IN " + thingy.me + "!");
