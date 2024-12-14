from dataclasses import dataclass
from queue import Empty, Queue
from typing import Any


@dataclass(frozen=True)
class Position:
    row: int
    col: int


def find_increasing_neighbors(pos: Position, grid) -> list[Position]:
    value = grid[pos.row][pos.col]
    neighbors = [
        Position(pos.row - 1, pos.col),
        Position(pos.row + 1, pos.col),
        Position(pos.row, pos.col - 1),
        Position(pos.row, pos.col + 1),
    ]
    return [
        p for p in neighbors
        if 0 <= p.row < len(grid) and 0 <= p.col < len(grid[0]) and grid[p.row][p.col] == value + 1
    ]


def find_trailheads(grid) -> list[Position]:
    trailheads: list[Position] = []

    for row, values in enumerate(grid):
        for col, value in enumerate(values):
            if value == 0:
                trailheads.append(Position(row=row, col=col))
            
    return trailheads
    

def main(grid) -> int:
    total_score = 0
    trailheads = find_trailheads(grid) 

    for th in trailheads:
        q = Queue()
        q.put(th)
        visited = set() 
        th_score = 0

        while not q.empty():
            path = q.get()

            if path in visited:
                continue

            visited.add(path)

            if grid[path.row][path.col] == 9:
                th_score += 1

            for neighbor in find_increasing_neighbors(path, grid):
                if neighbor not in visited:
                    q.put(neighbor)

        total_score += th_score
    return total_score


def parse_input_file(file) -> list[list[int]]:
    """Return a 2D-list of ints parsed from the given file."""
    with open(file, "r") as f:
        lines = f.readlines()

    grid = []
    for line in lines:
        grid.append([int(n) for n in list(line.strip("\n"))])

    return grid


if __name__ == "__main__":
    import argparse
    from pathlib import Path

    parser = argparse.ArgumentParser()
    parser.add_argument("input_file")
    args = parser.parse_args()

    grid = parse_input_file(Path(args.input_file))

    total_score = main(grid)
    print(f"Part One Score: {total_score}")
