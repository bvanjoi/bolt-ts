// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowDestructuringLoop.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface NumVal { val: number; }
interface StrVal { val: string; }
type Val = NumVal | StrVal;

function isNumVal(x: Val): x is NumVal {
    return typeof x.val === 'number';
}

function foo(things: Val[]): void {
    for (const thing of things) {
        if (isNumVal(thing)) {
            const { val } = thing;
            val.toFixed(2);
        }
        else {
            const { val } = thing;
            val.length;
        }
    }
}