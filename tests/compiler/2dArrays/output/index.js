class Cell {}
class Ship {
  isSunk = false;
}
class Board {
  ships = [];
  cells = [];
  allShipsSunk() {
    return this.ships.every(function (val) {
      return val.isSunk;
    });
  }
}