// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentsOnPropertyOfObjectLiteral1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var resolve = {
  id:/*! @ngInject */  (details) => (details.id),
  id1:/* c1 */  'hello',
  id2: /*! @ngInject */ (details) => (details.id),
  id3: /*! @ngInject */
  (details) => (details.id),
  id4: /*! @ngInject */
  /* C2 */
  (details) => (details.id)  
};