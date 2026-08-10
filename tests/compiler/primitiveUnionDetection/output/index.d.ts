type Kind = "one" | "two" | "three";
declare function getInterfaceFromString<T extends Kind>(options?: {
  type?: T;
} & {
  type?: Kind;
}): T;
declare var result: "two";
