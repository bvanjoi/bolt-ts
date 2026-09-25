// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveConditionalCrash4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

// Repros from #53783

type LengthDown<
  Str extends string,
  Length extends number | bigint,
  It
> = It extends StrIter.Iterator
//~^ ERROR: Cannot find name 'StrIter'.
  ? StrIter.CutAt<Str, It> extends `${infer $Rest}`
//~^ ERROR: Cannot find name 'StrIter'.
    ? LengthDown<$Rest, Add<Length, StrIter.Value<It>>, It>
//~^ ERROR: Cannot find name 'StrIter'.
//~| ERROR: Cannot find name 'Add'.
    : LengthDown<Str, Length, StrIter.Prev<It>>
//~^ ERROR: Cannot find name 'StrIter'.
  : Length;

type Foo<T> = T extends unknown
  ? unknown extends `${infer $Rest}`
    ? Foo<T>
    : Foo<unknown>
    //~^ ERROR: Type instantiation is excessively deep and possibly infinite.
    //~| ERROR: Type instantiation is excessively deep and possibly infinite.
  : unknown;
