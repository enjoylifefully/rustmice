from pyswip import Prolog
from random import randint


def generate_grid():
    side = randint(6, 10)
    grid = [["floor" for _ in range(side)] for _ in range(side)]

    for i in range(side):
        for j in range(side):
            cell = "floor"
            if i == 0 and j == 0:
                cell = "mouse"
            elif i == side - 1 and j == side - 1:
                cell = "cheese"
            else:
                match randint(0, 10):
                    case 0:
                        cell = "wall"
                    case 1:
                        cell = "box"
                    case 2:
                        cell = "cat"

            grid[i][j] = cell

    # Essa funfa
    # side = 5
    # grid = [
    #     ["mouse", "wall", "floor", "floor", "floor"],
    #     ["floor", "floor", "floor", "cat", "floor"],
    #     ["floor", "wall", "box", "floor", "wall"],
    #     ["wall", "cat", "floor", "floor", "cheese"],
    #     ["wall", "wall", "wall", "wall", "wall"],
    # ]

    # side = 7
    # grid = [
    #     ["mouse", "floor", "floor", "floor", "floor", "cat", "floor"],
    #     ["wall", "floor", "floor", "wall", "wall", "floor", "wall"],
    #     ["floor", "wall", "floor", "floor", "box", "floor", "floor"],
    #     ["wall", "wall", "floor", "cat", "floor", "floor", "wall"],
    #     ["box", "floor", "floor", "floor", "floor", "cat", "wall"],
    #     ["floor", "cat", "wall", "wall", "floor", "box", "floor"],
    #     ["floor", "floor", "floor", "floor", "floor", "floor", "cheese"],
    # ]

    # Essa falha
    # side = 3
    # grid = [
    #     ["mouse", "wall", "wall"],
    #     ["floor", "floor", "wall"],
    #     ["cat", "floor", "cheese"],
    # ]

    symbols = {
        "floor": "⬜",
        "wall": "⬛",
        "mouse": "🐭",
        "cheese": "🧀",
        "cat": "🐈",
        "box": "📦",
    }

    for row in grid:
        print("".join(symbols.get(cell, "?") for cell in row))

    prolog = Prolog()

    prolog.retractall("grid(_,_,_)")

    for i in range(side):
        for j in range(side):
            fact = f"grid({i}, {j}, {grid[i][j]})"
            prolog.assertz(fact)

    prolog.consult("solver.pl")

    solutions_generator = prolog.query("solve(X)")
    solutions = [sol["X"] for sol in solutions_generator if "X" in sol]

    if not solutions:
        print("nenhuma solução encontrada pelo Prolog.")
    else:
        print(f"Encontradas {len(solutions)} soluções.")
        for i, sol in enumerate(solutions):
            print(f"Solução {i + 1}: {sol}")

    return (grid, solutions)
