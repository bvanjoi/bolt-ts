// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nonNullReferenceMatching.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class Component {
  props;
  thumbYElementRef = (ref) => {
    typeof this.props.thumbYProps.elementRef === 'function' && this.props.thumbYProps.elementRef(ref);
    typeof (this.props.thumbYProps.elementRef) === 'function' && this.props.thumbYProps.elementRef(ref);
    typeof ((this.props).thumbYProps.elementRef) === 'function' && this.props.thumbYProps.elementRef(ref);
    typeof this.props.thumbXProps.elementRef === 'function' && this.props.thumbXProps.elementRef(ref);
    typeof this.props.thumbXProps.elementRef === 'function' && (this.props).thumbXProps.elementRef(ref);
    typeof this.props.thumbXProps.elementRef === 'function' && (this.props.thumbXProps).elementRef(ref);
    typeof this.props.thumbXProps.elementRef === 'function' && ((this.props).thumbXProps).elementRef(ref);
    typeof (this.props.thumbXProps).elementRef === 'function' && ((this.props).thumbXProps).elementRef(ref);
    typeof this.props.thumbXProps.elementRef === 'function' && ((this.props).thumbXProps).elementRef(ref);
  };
}
function hoho(b) {
  typeof b.f === 'function' && b.f();
}