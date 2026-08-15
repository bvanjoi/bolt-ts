// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/thisInTypeQuery.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

function assert(condition: unknown): asserts condition {
    if (!condition) {
        throw new Error();
    }
}

class MyClass {
    private map = {
        my_key: 'example_value'
    };

    runTypeFails() {
        const params = null as any as { a: { key: string } } | null;
        assert(params);
        type Key = keyof typeof this.map;
        this.map[params.a.key as Key];
    }
}

class C {
  foo() {
    const x = !!true;
    if (x) {
      type T0 = typeof this;
    }
  }
}