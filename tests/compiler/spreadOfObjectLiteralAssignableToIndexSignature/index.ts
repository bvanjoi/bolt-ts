// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/spreadOfObjectLiteralAssignableToIndexSignature.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

const foo: Record<never, never> = {} // OK
const bar:{ foo?: Record<never, never> } = {...(foo !== undefined && {foo})}

interface RecordOfRecords extends Record<keyof any, RecordOfRecords> {}
const recordOfRecords: RecordOfRecords = {}
recordOfRecords.propA = {...(foo !== undefined ? {foo} : {})} // OK
recordOfRecords.propB = {...(foo && {foo})} // OK
recordOfRecords.propC = {...(foo !== undefined && {foo})} // error'd in 3.7 beta, should be OK

interface RecordOfRecordsOrEmpty extends Record<keyof any, RecordOfRecordsOrEmpty | {}> {}
const recordsOfRecordsOrEmpty: RecordOfRecordsOrEmpty = {}
recordsOfRecordsOrEmpty.propA = {...(foo !== undefined ? {foo} : {})} // OK
recordsOfRecordsOrEmpty.propB = {...(foo && {foo})} // OK
recordsOfRecordsOrEmpty.propC = {...(foo !== undefined && {foo})} // OK