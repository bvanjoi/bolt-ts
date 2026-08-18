// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitOptionalMappedTypePropertyNoStrictNullChecks1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
//@compiler-options: declaration
import createApi from './createApi'
var slice = createApi({
  endpoints: {
      test: {
          url: `/user`      
    }    
  }  
});
var {useTestQuery} = slice;