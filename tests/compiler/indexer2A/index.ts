// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexer2A.ts`, Apache-2.0 License

//@compiler-options: target=es2015


class IHeapObjectProperty { }
class IDirectChildrenMap {
    // Decided to enforce a semicolon after declarations
    hasOwnProperty(objectId: number): boolean
    //~^ ERROR: Function implementation is missing or not immediately following the declaration.
    [objectId: number]: IHeapObjectProperty[]
}
var directChildrenMap = <IDirectChildrenMap>{}; 