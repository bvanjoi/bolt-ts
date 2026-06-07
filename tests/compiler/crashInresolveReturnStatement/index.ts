// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/crashInresolveReturnStatement.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class WorkItemToolbar {
    public onToolbarItemClick() {
        WITDialogs.createCopyOfWorkItem();
    }
}
class CreateCopyOfWorkItemDialog {
    public getDialogResult() {
        return null;
    }
}
function createWorkItemDialog<P0>(dialogType: P0) {
}
class WITDialogs {
    public static createCopyOfWorkItem() {
        createWorkItemDialog(CreateCopyOfWorkItemDialog);
    }
}
