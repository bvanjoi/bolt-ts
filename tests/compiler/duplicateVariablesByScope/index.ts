// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/duplicateVariablesByScope.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowUnreachableCode

namespace M {
    for (var j = 0; j < 10; j++) {
    }

    for (var j = 0; j < 10; j++) {
    }
}

function foo() {
    var x = 2;
    var x = 1;
    if (true) {
        var result = 1;
    }
    else {
        var result = 2;
    }
}

class C {
    foo() {
        try {
            var x = 1;
        }
        catch (e) {
            var x = 2;
        }
    }
}