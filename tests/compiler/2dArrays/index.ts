// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/2dArrays.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Cell {
}

class Ship {
    isSunk: boolean = false;
}

class Board {
    ships: Ship[] = [];
    cells: Cell[] = []

    private allShipsSunk() {
        return this.ships.every(function (val) { return val.isSunk; });
    }    
}
