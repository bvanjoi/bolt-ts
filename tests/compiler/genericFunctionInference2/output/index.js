
var myReducer1 = combineReducers({
  combined: combineReducers({
      foo    
  })  
});
var myReducer2 = combineReducers({
  combined: combineReducers({
      foo    
  })  
});
var enhancer4 = withH((props) => (({
  onChange: (props) => ((e) => {}),
  onSubmit: (props) => ((e) => {})  
})));
enhancer4.onChange(null);