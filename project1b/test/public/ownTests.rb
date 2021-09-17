require "minitest/autorun"
require_relative "../../src/controllers/input_controller.rb"
require_relative "../../src/controllers/game_controller.rb"
require_relative "../../src/models/game_board.rb"
require_relative "../../src/models/position.rb"
require_relative "../../src/models/ship.rb"

# The ship coordinates for p1, p2
SHIPS_P1 = "#{__dir__}/inputs/correct_ships_p1.txt"
SHIPS_P2 = "#{__dir__}/inputs/correct_ships_p2.txt"

# The attack coordinates against p1, p2
ATTACK_P1 = "#{__dir__}/inputs/correct_strat_p1.txt"
ATTACK_P2 = "#{__dir__}/inputs/correct_strat_p2.txt"

# The perfect attack coordinates against p1, p2
PERF_ATK_P1 = "#{__dir__}/inputs/perfect_strat_p1.txt"
PERF_ATK_P2 = "#{__dir__}/inputs/perfect_strat_p2.txt"

# A bad ships file
BAD_SHIPS = "#{__dir__}/inputs/bad_ships.txt"


test_board = GameBoard.new 10, 10

pos0 = Position.new(1, 1)
pos1 = Position.new(1, 2)
pos2 = Position.new(8, 8)


# Adding Ship
ship1 = Ship.new(pos0, "Down", 2)
ship2 = Ship.new(pos1, "Right", 9)
ship3 = Ship.new(pos2, "Up", 5)

test_board.add_ship(ship1)
# test_board.add_ship(ship2)
# test_board.add_ship(ship3)

test_board.attack_pos(Position.new(1, 1))
test_board.attack_pos(Position.new(2, 1))
test_board.attack_pos(Position.new(3, 1))



# print(test_board.to_s)
# print(test_board.num_successful_attacks)


board_p1 = read_ships_file(SHIPS_P1)

p2_moves = read_attacks_file(ATTACK_P1)

board_p1.attack_pos(p2_moves[0])
print(board_p1)

