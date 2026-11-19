// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/getParameterNameAtPosition.ts`, Apache-2.0 License
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
class GetSetEnumerableClassGet {
  get prop() {
    return true;
  }
}
class GetSetEnumerableClassSet {
  set prop(value) {}
}
class GetSetEnumerableClassGetSet {
  get prop() {
    return true;
  }
  set prop(value) {}
}
var GetSetEnumerableObjectGet = {
  get prop() {
    return true;
  }  
};
var GetSetEnumerableObjectSet = {
  set prop(value) {}  
};
var GetSetEnumerableObjectGetSet = {
  get prop() {
    return true;
  },
  set prop(value) {}  
};