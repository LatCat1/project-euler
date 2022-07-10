def blocks(length, minlength, mem = {}):
    if (length, minlength) in mem:
        return mem[(length, minlength)]
    if length == 0:
        count = 1
    elif length == -1:
        count = 1
    else:
        count = blocks(length - 1, minlength, mem=mem) + \
                sum(blocks(length - i - 1,minlength, mem) for i in range(minlength, length+1))
    mem[length] = count
    return count

mem = {}
for q in range(165, 168):
    print(f"{q}: {blocks(q, 50, mem)}")