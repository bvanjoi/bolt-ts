// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/shadowedReservedCompilerDeclarationsWithNoEmit.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: noEmit

// Shadowed captured this and super
class Base { }
class C extends Base {
    constructor() {
        super();
    }

    foo() {
        let _this = this;

        () => {
            _this;
        };
    }

    bar() {
        let _super = this;
    }
}


/// shadowed arguments
function f1(arguments, ...a) {
  //~^ ERROR: Invalid use of 'arguments'. Modules are automatically in strict mode.
}

class C2 extends Base {
    constructor() {
        super();
        var _newTarget = "";
        var t = new.target;
        var y = _newTarget;
    }
}


// Shadowed Promise
var Promise = null;
async function f4() {
    return 0;
}


// shadowed require
var require = 0;

// shadowed exports
var exports = 0;


export { };