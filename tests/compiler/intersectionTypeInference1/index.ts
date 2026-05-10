// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/intersectionTypeInference1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: useDefineForClassFields

function alert(s: string) {}

const parameterFn = (props:{store:string}) => alert(props.store)
const brokenFunction = <OwnProps>(f: (p: {dispatch: number} & OwnProps) => void) => (o: OwnProps) => o
export const Form3 = brokenFunction(parameterFn)({store: "hello"})
