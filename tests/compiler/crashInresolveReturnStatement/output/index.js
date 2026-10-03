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