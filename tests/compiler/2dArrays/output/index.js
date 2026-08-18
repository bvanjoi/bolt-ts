// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/2dArrays.ts`, Apache-2.0 License
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