// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursivelySpecializedConstructorDeclaration.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var MsPortal = {};
(function (MsPortal) {

  var Controls = {}// Removing this line fixes the constructor of ItemValue
  ;
  (function (Controls) {
  
    var Base = {};
    (function (Base) {
    
      var ItemList = {};
      (function (ItemList) {
      
        class ItemValue {
          constructor(value) {}
        }
        ItemList.ItemValue = ItemValue;
        
        class ViewModel extends ItemValue {}
        ItemList.ViewModel = ViewModel;
        
      })(ItemList);
      Base.ItemList = ItemList;
      
    })(Base);
    Controls.Base = Base;
    
  })(Controls);
  MsPortal.Controls = Controls;
  
})(MsPortal);