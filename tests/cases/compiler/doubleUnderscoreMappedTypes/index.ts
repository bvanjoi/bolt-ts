// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/doubleUnderscoreMappedTypes.ts`, Apache-2.0 License

interface Properties {
  property1: string;
  __property2: string;
}

// As expected, I can make an object satisfying this interface
const ok: Properties = {
  property1: "",
  __property2: ""
};

// As expected, "__property2" is indeed a key of the type
type Keys = keyof Properties;
const k: Keys = "__property2"; // ok
const k2: Keys = "property1"; // ok

// This should be valid
type Property1Type = Properties["property1"];
type Property2Type = Properties["__property2"];


// And should work with partial
const partial: Partial<Properties> = {
  property1: "",
  __property2: ""
};


let property1: Property1Type = 'property1';
let property2: Property2Type = 'property2';