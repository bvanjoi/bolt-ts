///<reference path='voidAsNonAmbiguousReturnType_0.ts'/>
import fs = require("./voidAsNonAmbiguousReturnType_0");

function main() {
 fs.mkdirSync('test'); // should not error - return types are the same
}