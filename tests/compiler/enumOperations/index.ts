// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/enumOperations.ts`, Apache-2.0 License

enum Enum { None = 0 }
var enumType: Enum = Enum.None;
var numberType: number = 0;
var anyType: any = 0;
 
enumType ^ numberType;
numberType ^ anyType;
 
enumType & anyType;
enumType | anyType;
enumType ^ anyType;
~anyType;
enumType <<anyType;
enumType >>anyType;
enumType >>>anyType;
