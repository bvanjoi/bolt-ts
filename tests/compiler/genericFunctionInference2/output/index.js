// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericFunctionInference2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@run-fail

var myReducer1 = combineReducers({
  combined: combineReducers({
      foo    
  })  
});
var myReducer2 = combineReducers({
  combined: combineReducers({
      foo    
  })  
});// Repro from #30942

var enhancer4 = withH((props) => (({
  onChange: (props) => ((e) => {}),
  onSubmit: (props) => ((e) => {})  
})));
enhancer4.onChange(null);