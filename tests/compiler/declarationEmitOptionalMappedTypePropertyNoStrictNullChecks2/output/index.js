import createApi from './createApi'
var slice = createApi({
  endpoints: {
      test: {
          url: `/user`      
    }    
  }  
});
var {useTestQuery} = slice;