// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeofStripsFreshness.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

interface Collection<T> {
    elems: T[];
}
interface CollectionStatic {
    new <T>(): Collection<T>;
}
declare const Collection: CollectionStatic;

const ALL = "all";
type All = typeof ALL;

const result: Collection<All> = new Collection();

const ANOTHER = "another";
type Another = typeof ANOTHER;

type Both = Another | All;

const result2: Collection<Both> = new Collection();
