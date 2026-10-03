var MsPortal = {};
(function (MsPortal) {

  var Controls = {};
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