// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/recursiveTupleTypes1.ts`, Apache-2.0 License

interface Tree1 {
  children: [Tree1, Tree2];
}

interface Tree2 {
  children: [Tree2, Tree1];
}

let tree1: Tree1;
let tree2: Tree2;
tree1 = tree2;
tree2 = tree1;
