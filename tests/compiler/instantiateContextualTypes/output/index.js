function fn(values, value) {}

fn(handlers, (value) => (alert(value)));
new GenericComponent({
  initialValues: 12,
  nextValues: (val) => (12)  
});
useStringOrNumber('', (foo) => {});
var defaultState = {
  dummy: ''  
};
var NON_VOID_ACTION = 'NON_VOID_ACTION', VOID_ACTION = 'VOID_ACTION';
createReducer(defaultState, handler(NON_VOID_ACTION, (state, _payload) => (state)), handler(VOID_ACTION, (state) => (state)));

x.on('a', (a) => {});
var N1 = {};
(function (N1) {

  class InferFunctionTypes extends Component {}
  
  createElement(InferFunctionTypes, (foo) => ('' + foo));
  
  createElement2(InferFunctionTypes, [(foo) => ('' + foo)]);
  
})(N1);

passContentsToFunc(outerBoxOfString, (box) => (box.value));
class Interesting {
  compiles = () => (Promise.resolve().then(() => {
    if (1 < 2) {
      return 'SOMETHING'
    }
    
    return 'ELSE'
  }));
  doesnt = () => (Promise.resolve().then(() => ('ELSE')));
  slightlyDifferentErrorMessage = () => (Promise.resolve().then(() => {
    if (1 < 2) {
      return 'SOMETHING'
    }
    
    return 'SOMETHING'
  }));
}
var xx = invoke(() => (1));
var obj = {
  foo(bar) {}  
};
assignPartial(obj, {
  foo(...args) {}  
});