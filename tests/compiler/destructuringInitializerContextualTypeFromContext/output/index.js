// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuringInitializerContextualTypeFromContext.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=true
var Parent = ({children, name = 'Artemis', ...props}) => (Child({
  name,
  ...props  
}));
var Child = ({children, name = 'Artemis', ...props}) => (`name: ${name} props: ${JSON.stringify(props)}`);
f(([_1, _2 = undefined]) => (undefined));