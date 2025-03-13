// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeArgumentInferenceWithRecursivelyReferencedTypeAliasToTypeLiteral02.ts`, Apache-2.0 License

//@ run-fail

type TreeNode = {
  name: string;
  parent: TreeNode;
}

type TreeNodeMiddleman = {
  name: string;
  parent: TreeNode;
}

var nodes: TreeNodeMiddleman[];
nodes.map(n => n.name);
