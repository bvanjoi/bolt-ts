// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferRestArgumentsMappedTuple.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

type MyMappedType<Primitive extends any> = {
	primitive: Primitive;
};

type TupleMapperOld<Tuple extends any[]> = {
	[Key in keyof Tuple]: Tuple[Key] extends Tuple[number] ? MyMappedType<Tuple[Key]> : never;
};

type MyMappedTupleOld = TupleMapperOld<[string, number]>; // [MyMappedType<string>, MyMappedType<number>]

declare function extractPrimitivesOld<Tuple extends any[]>(...mappedTypes: TupleMapperOld<Tuple>): Tuple;

const myPrimitiveTupleOld: [string, number] = extractPrimitivesOld({ primitive: "" }, { primitive: 0 });

type TupleMapperNew<Tuple extends any[]> = {
	[Key in keyof Tuple]: MyMappedType<Tuple[Key]>;
};

type MyMappedTupleNew = TupleMapperNew<[string, number]>;

declare function extractPrimitivesNew<Tuple extends any[]>(...mappedTypes: TupleMapperNew<Tuple>): Tuple;

const myPrimitiveTupleNew: [string, number] = extractPrimitivesNew({ primitive: "" }, { primitive: 0 });
