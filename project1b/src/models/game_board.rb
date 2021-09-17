class GameBoard
    attr_reader :max_row, :max_column

    def initialize(max_row, max_column)
        @max_row = max_row
        @max_column = max_column
        @num_successful_attacks = 0
        @board = []
        @attacks = []
        for i in (0...max_row)
            @board.append([])
            @attacks.append([])
            for j in 0...max_column
                @board[i].append("-")
                @attacks[i].append("-")
            end
        end
    end

    # adds a Ship object to the GameBoard
    # returns Boolean
    # Returns true on successfully added the ship, false otherwise
    # Note that Position pair starts from 1 to max_row/max_column
    def add_ship(ship) 
        if test_if_can_add_ship(ship) == false
            return false
        end

        if ship.orientation != "Up" and ship.orientation != "Down" and ship.orientation != "Right" and ship.orientation != "Left"
            return false
        end  
        add_ship_aux(ship)

        return true
    end

    def add_ship_aux(ship)
        if ship.orientation == "Up"
            row = ship.start_position.row
            column = ship.start_position.column
            i = 0
            for i in 0...ship.size
                @board[row-1-i][column-1] = "S"
            end
        elsif ship.orientation == "Down"
            row = ship.start_position.row
            column = ship.start_position.column
            i = 0
            for i in 0...ship.size
                @board[row-1+i][column-1] = "S"
            end
        elsif ship.orientation == "Left"
            row = ship.start_position.row
            column = ship.start_position.column
            for i in 0...ship.size
                @board[row-1][column-1-i] = "S"
            end
        elsif ship.orientation == "Right"
            row = ship.start_position.row
            column = ship.start_position.column
            for i in 0...ship.size
                @board[row-1][column-1+i] = "S"
            end
        end             
    end       

    def test_if_can_add_ship(ship)
        if ship.orientation == "Up"
            row = ship.start_position.row
            column = ship.start_position.column
            for i in 0...ship.size
                if row-i > @max_row or row-i < 1 or
                column > @max_column or column < 1 or
                @board[row-1-i][column-1] == "S"
                    return false
                end
            end
        elsif ship.orientation == "Down"
            row = ship.start_position.row
            column = ship.start_position.column
            for i in 0...ship.size
                if row+i > @max_row or row+i < 1 or 
                column > @max_column or column < 1 or
                @board[row-1+i][column-1] == "S"
                    return false
                end
            end
        elsif ship.orientation == "Left"
            row = ship.start_position.row
            column = ship.start_position.column
            for i in 0...ship.size
                if row > @max_row or row < 1 or
                column > @max_column or column-i < 1 or
                @board[row-1][column-1-i] == "S"
                    return false
                end
            end
        elsif ship.orientation == "Right"
            row = ship.start_position.row
            column = ship.start_position.column
            for i in 0...ship.size
                if row > @max_row or row < 1 or
                column+i > @max_column or column+i < 1 or
                @board[row-1][column-1+i] == "S"
                    return false
                end
            end
        end             
        return true         
    end



    # return Boolean on whether attack was successful or not (hit a ship?)
    # return nil if Position is invalid (out of the boundary defined)
    def attack_pos(position)
        # check position
        if position.row > @max_row or position.row < 1 or
        position.column > @max_column or position.column < 1
            return nil
        end

        # update your grid
        if @board[position.row-1][position.column-1] == "S" and @attacks[position.row-1][position.column-1] == "-"
            @num_successful_attacks+=1
        end
        @attacks[position.row-1][position.column-1] = "A"
        

        # return whether the attack was successful or not
        return @board[position.row-1][position.column-1] == "S"
    end

    # Number of successful attacks made by the "opponent" on this player GameBoard
    def num_successful_attacks
        return @num_successful_attacks
    end

    # returns Boolean
    # returns True if all the ships are sunk.
    # Return false if at least one ship hasn't sunk.

    def all_sunk?
        for i in 0...@max_row
            for j in 0...@max_column
                if @board[i][j] == "S" and @attacks[i][j] == "-"
                    return false
                end
            end
        end
        return true
    end


    # String representation of GameBoard (optional but recommended)
    def to_s
        row = ""
        i = 0
        j = 0
        for i in 0...@max_row
            for j in 0...@max_column
                row += @board[i][j] + "," + @attacks[i][j] + " | "
            end
            row+="\n"
        end

        return row
    end
end
