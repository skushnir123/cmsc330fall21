class PhoneBook
    def initialize
        @entryList = []
    end

    def add(name, number, is_listed)
        if !checkIfValidNum(number)
            return false
        end
        
        entry = Entry.new
        entry.setVars(name, number, is_listed)
        if (lookupWithoutListing(name) == nil and (is_listed == false or lookupByNum(number)==nil)) 
            @entryList.append(entry)
            return true
        else
            return false
        end
    end

    def lookup(name)
        for i in 0...@entryList.length
            if @entryList[i].getName() == name and @entryList[i].getIsListed == true
                return @entryList[i].getNumber()
            end
        end
        return nil
    end

    def lookupWithoutListing(name)
        for i in 0...@entryList.length
            if @entryList[i].getName() == name
                return @entryList[i].getNumber()
            end
        end
        return nil
    end

    def lookupByNum(number)
        for i in 0...@entryList.length
            if @entryList[i].getNumber() == number and @entryList[i].getIsListed == true
                return @entryList[i].getName()
            end
        end
        return nil
    end

    def namesByAc(areacode)
        nums = []
        for i in 0...@entryList.length
            if @entryList[i].getNumber[0..2] == areacode
                nums.append(@entryList[i].getName())
            end
        end
        return nums
    end

    def checkIfValidNum(num)
        if num.length != 12
            return false
        end
        for i in 0...12
            if (i == 3 or i == 7) and num[i] != "-"
                return false
            end

            if (i != 3 and i != 7) and !(num[i] =~ /[0-9]/)
                return false
            end
        end

        return true
    end
end


class Entry
    def initialize
        @name = ""
        @number = ""
        @is_listed = false
    end
    def setVars(name,number, is_listed)
        @name = name
        @number = number
        @is_listed = is_listed
    end
    def getName
        return @name
    end

    def getNumber
        return @number
    end

    def getIsListed
        return @is_listed
    end
end

