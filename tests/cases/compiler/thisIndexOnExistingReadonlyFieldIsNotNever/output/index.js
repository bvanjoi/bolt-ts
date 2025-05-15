// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisIndexOnExistingReadonlyFieldIsNotNever.ts`, Apache-2.0 License
class Component {
  props
  state
}


class CoachMarkAnchorDecorator {
  decorateComponent(anchor) {
    return class CoachMarkAnchor extends Component {
      _onAnchorRef = (anchor) => {
        var anchorRef = this.props.anchorRef;
        if (anchorRef) {
          anchor.props.children;
          anchor.state;
          anchorRef(anchor);
        }
        
      }
    }
  }
}