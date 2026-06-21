// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexer2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface IHeapObjectProperty {}
interface IDirectChildrenMap { 
        hasOwnProperty(objectId: number) : boolean; 
        [objectId: number] : IHeapObjectProperty[]; 
}    
var directChildrenMap = <IDirectChildrenMap>{}; 
