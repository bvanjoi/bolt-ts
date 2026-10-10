// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/generics0.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

interface G<T> {
    x: T;
}

var v2: G<string>;

var z = v2.x; // 'y' should be of type 'string'
//~^ ERROR: Variable 'v2' is used before being assigned.


type Match<i> = {
  with<p extends Readonly<i>>(): Match<i>
};

declare function match<input>(
  value: input
): Match<input>;

type Input = {
  kind: number;
};

class Repro {
  method(input: Input) {
    const value = { ...input };
    match(value)
      .with()
      .with()
      .with()
      .with()
  }
}
