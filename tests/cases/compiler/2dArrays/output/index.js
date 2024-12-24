class Cell {}
class Ship {
  isSunk
}
class Board {
  ships
  cells
  allShipsSunk() {
    return this.ships.every(function (val) {
      return val.isSunk    
})  
}
}