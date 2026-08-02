// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/referenceSatisfiesExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015

let a = 10;
--(a satisfies number);
++(a satisfies number);

(a satisfies number)++;
(a satisfies number)--;

let b: number;
(b satisfies number) = 10;
//~^ ERROR: Variable 'b' is used before being assigned.

let c: number;
[(c satisfies number)] = [10];
//~^ ERROR: Variable 'c' is used before being assigned.
//~| ERROR: Variable 'c' is used before being assigned.

let d: number, e = 1;
({ d: (e satisfies number) } = { d: 10 });

let g = 1
for ((g satisfies number) of [10]) {
  console.log(g)
}

let x: string = "hello"
for ((x satisfies string) in { a: 10 }) {
  console.log(x)
}
