// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/nestedCallbackErrorNotFlattened.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type Cb<T> = {noAlias: () => T}["noAlias"]; // `"noAlias"` here prevents an alias symbol from being made
// which means the comparison will definitely be structural, rather than by variance

declare const x: Cb<Cb<Cb<Cb<number>>>>; // one more layer of `Cb` adn we'd get a `true` from the deeply-nested symbol check
declare let y: Cb<Cb<Cb<Cb<string>>>>;
y = x;
//~^ ERROR: Type '() => () => () => () => number' is not assignable to type '() => () => () => () => string'.