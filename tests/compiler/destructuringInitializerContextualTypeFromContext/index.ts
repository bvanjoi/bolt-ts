// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuringInitializerContextualTypeFromContext.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=true
//@run-fail

interface SFC<P = {}> {
    (props: P & { children?: any }): any | null;
}

interface Props {
    name: "Apollo" | "Artemis" | "Dionysus" | "Persephone";
}

const Parent: SFC<Props> = ({
    children,
    name = "Artemis",
    ...props
}) => Child({name, ...props});

const Child: SFC<Props> = ({
    children,
    name = "Artemis",
    ...props
}) => `name: ${name} props: ${JSON.stringify(props)}`;

// Repro from #29189

declare function f(g: (as: string[]) => void): void
f(([_1, _2 = undefined]) => undefined)
