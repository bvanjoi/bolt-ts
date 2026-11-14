enum E {
  regular = 0,
  'hyphen-member' = 1,
  '123startsWithNumber' = 2,
  'has space' = 3,
  Ϳ = 4
}
export var a: E["hyphen-member"];
export var b: E["123startsWithNumber"];
export var c: E["has space"];
export var d: E.regular;
export var e: E.Ϳ;
