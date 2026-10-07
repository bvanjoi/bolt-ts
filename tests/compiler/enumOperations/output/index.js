// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/enumOperations.ts`, Apache-2.0 License
var Enum = {};
(function (Enum) {

  Enum[Enum['None'] = 0] = 'None'
})(Enum);
var enumType = Enum.None;
var numberType = 0;
var anyType = 0;
enumType ^ numberType;
numberType ^ anyType;
enumType & anyType;
enumType | anyType;
enumType ^ anyType;
~anyType;
enumType << anyType;
enumType >> anyType;
enumType >>> anyType;