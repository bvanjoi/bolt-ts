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
      
    })(Base);
    
  })(Controls);
  
})(MsPortal);