var ActionType = {};
(function (ActionType) {

  ActionType[ActionType['Foo'] = 0] = 'Foo'
  ActionType[ActionType['Bar'] = 0] = 'Bar'
  ActionType[ActionType['Baz'] = 0] = 'Baz'
  ActionType[ActionType['Batch'] = 0] = 'Batch'
})(ActionType);
function assertNever(a) {
  throw new Error('Unreachable!')
}
function reducer(action) {
  switch (action.type) {
    case ActionType.Bar:
      var x = action.payload;
      break;
    
    case ActionType.Baz:
      var y = action.payload;
      break;
    
    case ActionType.Foo:
      var z = action.payload;
      break;
    
    case ActionType.Batch:
      action.payload.map(reducer);
      break;
    
    default:
      return assertNever(action)
    
  }
}