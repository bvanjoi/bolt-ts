// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/conditionalTypeClassMembers.ts`, Apache-2.0 License
//@compiler-options: target=es2015

declare class MyRecord {
    private a();
    b(): unknown;
}

declare class MySet<TSet extends MyRecord> {
    public item(): TSet;
}

type DS<TRec extends MyRecord | { [key: string]: unknown }> = TRec extends MyRecord ? MySet<TRec> : TRec[];