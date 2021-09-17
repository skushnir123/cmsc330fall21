require_relative '../models/game_board'
require_relative '../models/ship'
require_relative '../models/position'

# return a populated GameBoard or nil
# Return nil on any error (validation error or file opening error)
# If 5 valid ships added, return GameBoard; return nil otherwise
def read_ships_file(path)
    num_ships = 0;
    gameBoard = GameBoard.new 10, 10
    test = read_file_lines(path) {|line|
        if num_ships < 5
            if line =~ /^(\()([0-9]{1,2})(,)([0-9]{1,2})(\),)\s+[a-zA-z]+(,)\s[1-5]$/
                firstCommaIndex = line.index(",")
                secondCommaIndex = line[firstCommaIndex+1..-1].index(",") + firstCommaIndex+1
                thirdCommaIndex = line[secondCommaIndex+1..-1].index(",") + secondCommaIndex+1
                row = line[1...firstCommaIndex].to_i
                column = line[firstCommaIndex+1...secondCommaIndex-1].to_i
                pos = Position.new(row, column)
                orientation = line[secondCommaIndex+2...thirdCommaIndex]
                size = line[thirdCommaIndex+2..-1].to_i

                if size <=5 and size >= 1
                    ship = Ship.new(pos, orientation, size)
                    addedShip = gameBoard.add_ship(ship)
                    if addedShip != false
                        num_ships+=1
                    end
                end
            end
        end
    }
    if test == false or num_ships < 5
        return nil
    end
    return gameBoard
    
end


# return Array of Position or nil
# Returns nil on file open error
def read_attacks_file(path)
    positions = []
    test = read_file_lines(path) {|line| 
        firstCommaIndex = line.index(",")
        if firstCommaIndex != nil and line =~ /^(\()[0-9]{1,2},[0-9]{1,2}\)$/
            row = line[1...firstCommaIndex].to_i
            column = line[firstCommaIndex+1..-3].to_i
            pos = Position.new(row, column)
            positions.append(pos)
        end
    }
    if test == false
        return nil
    end
    
    return positions
end 


# ===========================================
# =====DON'T modify the following code=======
# ===========================================
# Use this code for reading files
# Pass a code block that would accept a file line
# and does something with it
# Returns True on successfully opening the file
# Returns False if file doesn't exist
def read_file_lines(path)
    return false unless File.exist? path
    if block_given?
        File.open(path).each do |line|
            yield line
        end
    end

    true
end
