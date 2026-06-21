// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reverseMappedTypeIntersectionConstraint.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type StateConfig<TAction extends string> = {
  entry?: TAction
  states?: Record<string, StateConfig<TAction>>;
};

type StateSchema = {
  states?: Record<string, StateSchema>;
};

declare function createMachine<
  TConfig extends StateConfig<TAction>,
  TAction extends string = TConfig["entry"] extends string ? TConfig["entry"] : string,
>(config: { [K in keyof TConfig & keyof StateConfig<any>]: TConfig[K] }): [TAction, TConfig];

const inferredParams1 = createMachine({
  entry: "foo",
  states: {
    a: {
      entry: "bar",
      //~^ ERROR: Type '"bar"' is not assignable to type 'undefined | "foo"'.
    },
  },
  extra: 12,
});

const inferredParams2 = createMachine({
  entry: "foo",
  states: {
    a: {
      entry: "foo",
    },
  },
  extra: 12,
  //~^ ERROR: Object literal may only specify known properties, and 'extra' does not exist in type '{ entry: TConfig["entry"]; states: TConfig["states"]; }'
});


// -----------------------------------------------------------------------------------------

const checkType = <T>() => <U extends T>(value: { [K in keyof U & keyof T]: U[K] }) => value;

const checked = checkType<{x: number, y: string}>()({
  x: 1 as number,
  y: "y",
  z: "z", // undesirable property z is *not* allowed
  //~^ ERROR: Object literal may only specify known properties, and 'z' does not exist in type '{ }'.
});

checked;

// -----------------------------------------------------------------------------------------

interface Stuff {
    field: number;
    anotherField: string;
}

function doStuffWithStuff<T extends Stuff>(s: { [K in keyof T & keyof Stuff]: T[K] } ): T {
    if(Math.random() > 0.5) {
      return s as T
    } else {
      return s
      //~^ ERROR: Type '{ field: T["field"]; anotherField: T["anotherField"]; }' is not assignable to type 'T'.
    }
}

doStuffWithStuff({ field: 1, anotherField: 'a', extra: 123 })
//~^ ERROR: Object literal may only specify known properties, and 'extra' does not exist in type '{ field: T["field"]; anotherField: T["anotherField"]; }'.

function doStuffWithStuffArr<T extends Stuff>(arr: { [K in keyof T & keyof Stuff]: T[K] }[]): T[] {
    if(Math.random() > 0.5) {
      return arr as T[]
    } else {
      return arr
      //~^ ERROR: Type '{ field: T["field"]; anotherField: T["anotherField"]; }[]' is not assignable to type 'T[]'.
    }
}

doStuffWithStuffArr([
    { field: 1, anotherField: 'a', extra: 123 },
    //~^ ERROR: Object literal may only specify known properties, and 'extra' does not exist in type '{ field: T["field"]; anotherField: T["anotherField"]; }'.
])

// -----------------------------------------------------------------------------------------

type XNumber = { x: number }

declare function foo<T extends XNumber>(props: {[K in keyof T & keyof XNumber]: T[K]}): void;

function bar(props: {x: number, y: string}) {
  return foo(props); // no error because lack of excess property check by design
}

foo({x: 1, y: 'foo'});
//~^ ERROR: Object literal may only specify known properties, and 'y' does not exist in type '{ x: T["x"]; }'.

foo({...{x: 1, y: 'foo'}}); // no error because lack of excess property check by design

// -----------------------------------------------------------------------------------------

type NoErrWithOptProps = { x: number, y?: string }

declare function baz<T extends NoErrWithOptProps>(props: {[K in keyof T & keyof NoErrWithOptProps]: T[K]}): void;

baz({x: 1});
baz({x: 1, z: 123});
//~^ ERROR: Object literal may only specify known properties, and 'z' does not exist in type '{ x: T["x"]; y: T["y"]; }'.
baz({x: 1, y: 'foo'});
baz({x: 1, y: 'foo', z: 123});
//~^ ERROR: Object literal may only specify known properties, and 'z' does not exist in type '{ x: T["x"]; y: T["y"]; }'.

// -----------------------------------------------------------------------------------------

interface WithNestedProp {
  prop: string;
  nested: {
    prop: string;
  }
}

declare function withNestedProp<T extends WithNestedProp>(props: {[K in keyof T & keyof WithNestedProp]: T[K]}): T;

const wnp = withNestedProp({prop: 'foo', nested: { prop: 'bar' }, extra: 10 });
//~^ ERROR: Object literal may only specify known properties, and 'extra' does not exist in type '{ prop: T["prop"]; nested: T["nested"]; }'.

// -----------------------------------------------------------------------------------------

type IsLiteralString<T extends string> = string extends T ? false : true;

type DeepWritable<T> = T extends Function ? T : { -readonly [K in keyof T]: DeepWritable<T[K]> }

interface ProvidedActor {
  src: string;
  logic: () => Promise<unknown>;
}

type DistributeActors<TActor> = TActor extends { src: infer TSrc }
  ? {
      src: TSrc;
    }
  : never;

interface MachineConfig<TActor extends ProvidedActor> {
  types?: {
    actors?: TActor;
  };
  invoke: IsLiteralString<TActor["src"]> extends true
    ? DistributeActors<TActor>
    : {
        src: string;
      };
}

type NoExtra<T> = {
  [K in keyof T]: K extends keyof MachineConfig<any> ? T[K] : never
}

declare function createXMachine<
  const TConfig extends MachineConfig<TActor>,
  TActor extends ProvidedActor = TConfig extends { types: { actors: ProvidedActor} } ? TConfig["types"]["actors"] : ProvidedActor,
>(config: {[K in keyof MachineConfig<any> & keyof TConfig]: TConfig[K] }): TConfig;

const child = () => Promise.resolve("foo");

const config = createXMachine({
  types: {} as {
    actors: {
      src: "str";
      logic: typeof child;
    };
  },
  invoke: {
    src: "str",
  },
  extra: 10
  //~^ ERROR: Object literal may only specify known properties, and 'extra' does not exist in type '{ types: TConfig["types"]; invoke: TConfig["invoke"]; }'.
});

const config2 = createXMachine({
  invoke: {
    src: "whatever",
  },
  extra: 10
  //~^ ERROR: Object literal may only specify known properties, and 'extra' does not exist in type '{ types: TConfig["types"]; invoke: TConfig["invoke"]; }'.
});
