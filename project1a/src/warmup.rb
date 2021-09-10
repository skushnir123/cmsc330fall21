def fib(n) 
    nNums = []
    i = 0
    while i < n
        if i == 0 or i == 1
            nNums.append(i)
        else
            nNums.append(nNums[i-1]+nNums[i-2])
        end
        i+=1
    end
    
    return nNums
end 

def isPalindrome(n)
    nString = n.to_s
    for i in 0...(nString.length/2)
        if nString[i] != nString[nString.length-1-i]
            return false
        end
    end
    return true
end

def nthmax(n, a)
    a.sort!
    if n >= a.length
        return nil
    end
    return a[a.length-n-1]
end

def freq(s)
    if s.length == 0
        return ""
    end

    mostFrequentChar = s[0] 
    charMap = {}
    for i in 0...(s.length)
        if charMap.has_key?(s[i])
            charMap[s[i]] = charMap[s[i]] + 1
        else
            charMap[s[i]] = 1
        end
        if charMap[s[i]] > charMap[mostFrequentChar]
            mostFrequentChar = s[i]
        end
    end
    return mostFrequentChar

end

def zipHash(arr1, arr2)
    arrMapping = {}
    if arr1.length != arr2.length
        return nil
    end
    for i in 0...(arr1.length)
        arrMapping[arr1[i]] = arr2[i]
    end
    return arrMapping
end

def hashToArray(hash)
    hashedArrays = []
    for i in 0...hash.keys.length
        hashedArrays.append([hash.keys[i], hash[hash.keys[i]]])
    end
    return hashedArrays
end
