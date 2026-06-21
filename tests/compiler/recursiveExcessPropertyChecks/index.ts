// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveExcessPropertyChecks.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

interface ITreeItem {    
  Parent?: this;
}

type NodeWithId = ITreeItem & { Id?: number };

function getMaxId(items: NodeWithId[]) {
}

const nodes = [] as ITreeItem[];
getMaxId(nodes);

