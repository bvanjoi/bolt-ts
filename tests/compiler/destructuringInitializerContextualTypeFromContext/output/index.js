// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuringInitializerContextualTypeFromContext.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=true
//@run-fail
var Parent = ({children, name = 'Artemis', ...props}) => (Child({
  name,
  ...props  
}));
var Child = ({children, name = 'Artemis', ...props}) => (`name: ${name} props: ${JSON.stringify(props)}`);// Repro from #29189

f(([_1, _2 = undefined]) => (undefined));