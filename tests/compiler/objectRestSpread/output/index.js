var obj = {};
({
  ...obj  
});
var {prop = {
  ...obj  
}, more = {
  ...obj  
} = {
  ...obj  
}, ['' + 'other']: other = {
  ...obj  
}, yetAnother: {nested: {['nested' + 'prop']: nestedProp = {
  ...obj  
}, ...nestedRest} = {
  ...obj  
}} = {
  ...obj  
}, fn = function* () {}, ...props} = {};
({
  prop,
  ['' + 'other']: other = {
      ...obj    
  },
  ...props  
} = {});
function test({prop = {
  ...obj  
}, ...props}) {}