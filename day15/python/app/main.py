from collections import namedtuple
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Any


class ParseError(Exception):
    pass


GridIndex = namedtuple("GridIndex", ['row', 'col'])


class Direction(Enum):
    UP = "^"
    DOWN = "v"
    LEFT = "<"
    RIGHT = ">"

    @property
    def inverse(self):
        match self:
          case Direction.UP:
                return Direction.DOWN
          case Direction.DOWN:
                return Direction.UP
          case Direction.LEFT:
                return Direction.RIGHT
          case Direction.RIGHT:
                return Direction.LEFT


    @classmethod
    def parse(cls, c: str):
        match c:
            case Direction.UP.value:
                return Direction.UP
            case Direction.DOWN.value:
                return Direction.DOWN
            case Direction.LEFT.value:
                return Direction.LEFT
            case Direction.RIGHT.value:
                return Direction.RIGHT
            case _:
                raise ParseError(f"Could not pase direction: {c}")


@dataclass
class Cell:
    index: GridIndex
    value: Any | None = None

    def get_neighbor_index(self, direction: Direction) -> GridIndex:
        match direction:
            case Direction.UP:
                return GridIndex(self.index.row - 1, self.index.col)
            case Direction.DOWN:
                return GridIndex(self.index.row + 1, self.index.col)
            case Direction.LEFT:
                return GridIndex(self.index.row, self.index.col - 1)
            case Direction.RIGHT:
                return GridIndex(self.index.row - 1, self.index.col + 1)


class GridGlyph(Enum):
    WALL = "#"
    OPEN = "."
    BOX = "O"
    ROBOT = "@"

    @classmethod
    def parse(cls, c: str):
        match c:
            case GridGlyph.WALL.value:
                return GridGlyph.WALL
            case GridGlyph.OPEN.value:
                return GridGlyph.OPEN
            case GridGlyph.BOX.value:
                return GridGlyph.BOX
            case GridGlyph.ROBOT.value:
                return GridGlyph.ROBOT
            case _:
                raise ParseError(f"Could not parse grid glyph: {c}")

    def __repr__(self):
        return self.value

    def __str__(self):
        return self.value


class Grid:
    def __init__(self, x: int, y: int):
        self._grid = [[Cell(index=GridIndex(row, col)) for col in range(x)] for row in range(y)] 

    def set_cell(self, row: int, col: int, value: Any):
        self._grid[row][col] = Cell(index=GridIndex(row, col), value=value)

    def get_cell(self, index: GridIndex) -> Cell:
        return self._grid[index.row][index.col]

    def find_cell(self, value: Any) -> Cell | None:
        for row in self._grid:
            for cell in row:
                if cell.value == value:
                    return cell
        return None

    def __repr__(self):
        grid = ""
        for row in self._grid:
            for cell in row:
                grid += str(cell.value) if cell.value else " "
            grid += "\n"
        return grid
                    


def parse_input(file: Path) -> (Grid, list[Direction]):
    with open(file, "r") as f:
        lines = f.readlines()

    raw_input = [line.strip("\n") for line in lines]
    split = raw_input.index('')
    top_edge = raw_input[0]

    inital_grid_raw = raw_input[:split]
    robot_moves_raw = raw_input[split+1:-1]

    width = len(top_edge)
    height = len(inital_grid_raw)
    grid = Grid(width, height)

    for row, values in enumerate(inital_grid_raw):
        for col, value in enumerate(values):
            grid.set_cell(row, col, GridGlyph.parse(value))

    moves = []
    for row in robot_moves_raw:
        for col in row:
            moves.append(Direction.parse(col))

    return grid, moves


class WarehouseManager:
    def __init__(self, grid: Grid):
        self.grid = grid

    def _locate_robot(self) -> Cell | None:
        return self.grid.find_cell(GridGlyph.ROBOT)

    def _gps_score(self, cell) -> int:
        return 4 * cell.index.row + cell.index.col

    @property
    def gps_scores(self) -> list[int]:
        return [self._gps_score(cell) for row in self.grid._grid for cell in row if cell.value == GridGlyph.BOX]

    def move_robot(self, direction: Direction):
        current_cell = self._locate_robot()
        assert current_cell != None, "Unable to locate Robot!"

        boxes = []
        while current_cell.value != GridGlyph.OPEN:
            if current_cell.value == GridGlyph.WALL:
                return
            if current_cell.value == GridGlyph.BOX:
                boxes.append(current_cell)
            current_cell = self.grid.get_cell(current_cell.get_neighbor_index(direction))
        
        for box in reversed(boxes):
            box.value = self.grid.get_cell(current_cell.get_neighbor_index(direction.inverse))


def main(input_file: Path):
    grid, moves = parse_input(input_file)

    wh = WarehouseManager(grid)
    for move in moves:
        wh.move_robot(move)

    print(f"GPS Scores: {sum(wh.gps_scores)}")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("file_name", )

    args = parser.parse_args()
    input_file = Path(args.file_name)
    if input_file.exists():
        main(input_file)
    else:
        raise ValueError(f"Input file: {args.file_name} does not exist.")
