from random import randint
from pyswip import Prolog

# side = randint(2, 5)
# grid = [["floor" for _ in range(side)] for _ in range(side)]
side = 5
grid = [
    ["mouse", "wall", "floor", "floor", "floor"],
    ["floor", "floor", "floor", "cat", "floor"],
    ["floor", "wall", "box", "floor", "wall"],
    ["wall", "cat", "floor", "floor", "cheese"],
    ["wall", "wall", "wall", "wall", "wall"],
]

# for i in range(side):
#     for j in range(side):
#         cell = "floor"

#         if i == 0 and j == 0:
#             cell = "mouse"
#         elif i == side - 1 and j == side - 1:
#             cell = "cheese"
#         else:
#             match randint(0, 10):
#                 case 0:
#                     cell = "wall"
#                 case 1:
#                     cell = "box"
#                 case 2:
#                     cell = "cat"

#         grid[i][j] = cell

prolog = Prolog()

for i in range(side):
    for j in range(side):
        fact = f"grid({i}, {j}, {grid[i][j]})"
        prolog.assertz(fact)

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


prolog.consult("solver.pl")

for path in Prolog.query("solve(X)"):
    print(path["X"], "\n")
