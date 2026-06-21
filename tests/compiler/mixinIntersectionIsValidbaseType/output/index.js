var Serializable = (SuperClass) => {
  var LocalMixin = (InnerSuperClass) => (class SerializableLocal extends InnerSuperClass {});
  var ResultClass = LocalMixin(SuperClass);
  return ResultClass
};
var AMixin = (SuperClass) => {
  var SomeHowOkay = class A extends SuperClass {};
  var SomeHowNotOkay = class A extends Serializable(SuperClass) {};
};