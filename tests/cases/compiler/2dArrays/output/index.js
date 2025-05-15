// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/2dArrays.ts`, Apache-2.0 License
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