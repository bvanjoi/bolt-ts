// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowForCompoundAssignmentToThisMember.ts`, Apache-2.0 License
class DatasourceCommandWidgetElement {
  _commandBased;
  _commandElement;
  commandElement;
  constructor(target) {if (target instanceof DatasourceCommandWidgetElement) {
      this._commandBased = true;
      this._commandElement = target.commandElement;
    } else {
      this._commandBased = false;
    }
    
    if (this._commandBased = (target instanceof DatasourceCommandWidgetElement)) {
      this._commandElement = target.commandElement;
    }
    }
}