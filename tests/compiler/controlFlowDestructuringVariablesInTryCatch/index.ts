// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowDestructuringVariablesInTryCatch.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

declare function f1(): string;
declare function f2(): [b: string];
declare function f3(): { c: string };

try {
    var a = f1();
    var [b] = f2();
    var { c } = f3();

    var [d = 1] = [];
    var { e = 1 } = { };
} catch {
    console.error("error");
}

a;
//~^ ERROR: Variable 'a' is used before being assigned.
b;
//~^ ERROR: Variable 'b' is used before being assigned.
c;
//~^ ERROR: Variable 'c' is used before being assigned.
d;
//~^ ERROR: Variable 'd' is used before being assigned.
e;
//~^ ERROR: Variable 'e' is used before being assigned.
