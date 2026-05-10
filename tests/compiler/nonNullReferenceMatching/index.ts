// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nonNullReferenceMatching.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type ElementRef = (element: HTMLElement | null) => void;

type ThumbProps = {
    elementRef?: ElementRef;
}

type ComponentProps = {
    thumbYProps?: ThumbProps;
    thumbXProps: ThumbProps;
}

class Component {
    props!: ComponentProps;
    public thumbYElementRef = (ref: HTMLElement | null) => {
        typeof this.props.thumbYProps!.elementRef === 'function' && this.props.thumbYProps!.elementRef(ref);

        typeof (this.props.thumbYProps!.elementRef) === 'function' && this.props.thumbYProps!.elementRef(ref);

        typeof ((this.props).thumbYProps!.elementRef)! === 'function' && this.props.thumbYProps!.elementRef(ref);

        typeof this.props.thumbXProps.elementRef === 'function' && this.props.thumbXProps.elementRef(ref);

        typeof this.props.thumbXProps.elementRef === 'function' && (this.props).thumbXProps.elementRef(ref);

        typeof this.props.thumbXProps.elementRef === 'function' && (this.props.thumbXProps).elementRef(ref);

        typeof this.props.thumbXProps.elementRef === 'function' && ((this.props)!.thumbXProps)!.elementRef(ref);

        typeof (this.props.thumbXProps).elementRef === 'function' && ((this.props)!.thumbXProps)!.elementRef(ref);

        typeof this.props!.thumbXProps!.elementRef === 'function' && ((this.props)!.thumbXProps)!.elementRef(ref);
    };
}

function hoho(b: { f?: () => void } | undefined) {
  typeof b!.f === 'function' && b!.f();
}