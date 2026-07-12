// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowForCompoundAssignmentToThisMember.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class DatasourceCommandWidgetElement {
    _commandBased: boolean;
    _commandElement: unknown;
    commandElement: unknown;

    constructor(target: unknown) {
        if (target instanceof DatasourceCommandWidgetElement) {
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
