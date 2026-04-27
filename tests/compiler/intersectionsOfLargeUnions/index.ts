// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/intersectionsOfLargeUnions.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

export function assertIsElement(node: Node | null): node is Element {
    let nodeType = node === null ? null : node.nodeType;
    return nodeType === 1;
}
  
export function assertNodeTagName<
    T extends keyof ElementTagNameMap,
    U extends ElementTagNameMap[T]>(node: Node | null, tagName: T): node is U {
    if (assertIsElement(node)) {
        const nodeTagName = node.tagName.toLowerCase();
         return nodeTagName === tagName;
    }
    return false;
}
  
export function assertNodeProperty<
    T extends keyof ElementTagNameMap,
    P extends keyof ElementTagNameMap[T],
    V extends HTMLElementTagNameMap[T][P]>(node: Node | null, tagName: T, prop: P, value: V) {
      //~^ ERROR: Type 'T' cannot be used to index type 'HTMLElementTagNameMap'.
      //~| ERROR: Type 'P' cannot be used to index type 'HTMLElementTagNameMap[T]'.
    if (assertNodeTagName(node, tagName)) {
        node[prop];
    }
}
