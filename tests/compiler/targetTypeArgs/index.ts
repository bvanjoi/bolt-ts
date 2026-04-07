// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/targetTypeArgs.ts`, Apache-2.0 License

function foo(callback: (x: string) => void) {
    callback("hello");   
}

foo(function(x) { x });

[1].forEach(function(v,i,a) { v });
["hello"].every(function(v,i,a) {return true;});
[1].every(function(v,i,a) {return true;});
[1].every(function(v,i,a) {return true;});
["s"].every(function(v,i,a) {return true;});
["s"].forEach(function(v,i,a) { v });

