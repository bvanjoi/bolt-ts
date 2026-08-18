// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/crashInresolveReturnStatement.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class WorkItemToolbar {
  onToolbarItemClick() {
    WITDialogs.createCopyOfWorkItem();
  }
}
class CreateCopyOfWorkItemDialog {
  getDialogResult() {
    return null;
  }
}
function createWorkItemDialog(dialogType) {}
class WITDialogs {
  static createCopyOfWorkItem() {
    createWorkItemDialog(CreateCopyOfWorkItemDialog);
  }
}