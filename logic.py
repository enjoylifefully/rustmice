from pyswip import Prolog
from random import randint
from enum import Enum


class Cell(Enum):
    BOX = "box"
    CAT = "cat"
    CHEESE = "cheese"
    FLOOR = "floor"
    RAT = "rat"
    WATER = "water"


class Action(Enum):
    UP = "up"
    DOWN = "down"
    LEFT = "left"
    RIGHT = "right"


SYMBOLS = {
    Cell.BOX: "📦",
    Cell.CAT: "🐈",
    Cell.CHEESE: "🧀",
    Cell.FLOOR: "⬜",
    Cell.RAT: "🐭",
    Cell.WATER: "💧",
}

Grid = list[list[Cell]]
Solution = list[Action]


def generate_grid() -> Grid:
    side = randint(6, 10)
    grid = [[Cell.FLOOR for _ in range(side)] for _ in range(side)]

    for x in range(side):
        for y in range(side):
            cell = Cell.FLOOR

            if x == 0 and y == 0:
                cell = Cell.RAT
            elif x == side - 1 and y == side - 1:
                cell = Cell.CHEESE
            else:
                match randint(0, 10):
                    case 0:
                        cell = Cell.BOX
                    case 1:
                        cell = Cell.CAT
                    case 2:
                        cell = Cell.WATER
                    case _:
                        pass

            grid[x][y] = cell

    return grid


def print_grid(grid: Grid):
    for row in grid:
        print("".join(SYMBOLS[cell] for cell in row))


def get_solution(grid: Grid):
    prolog = Prolog()

    prolog.retractall("grid(_,_,_)")

    for x, row in enumerate(grid):
        for y, cell in enumerate(row):
            fact = f"grid({x}, {y}, {cell.value})"
            prolog.assertz(fact)

    prolog.consult("solver.pl")

    query = prolog.query("solve(X)")

    try:
        solution = [Action(action) for action in next(query)["X"]]

        return solution
    except StopIteration:
        return None


def print_solution(maybe_solution: None | Solution):
    if maybe_solution is None:
        print("nenhuma solução encontrada pelo prolog")
    else:
        print("solução encontrada pelo prolog")

        for i, action in enumerate(maybe_solution):
            print(f"passo {i + 1}: {action}")
