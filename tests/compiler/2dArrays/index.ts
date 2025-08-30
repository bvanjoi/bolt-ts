// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/2dArrays.ts`, Apache-2.0 License

class Cell {
}

class Ship {
    isSunk: boolean;
}

class Board {
    ships: Ship[];
    cells: Cell[];

    private allShipsSunk() {
        return this.ships.every(function (val) { return val.isSunk; });
    }    
}
