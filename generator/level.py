s = """#####
#@x*#
#x#$#
#*$$#
#####
"""

s = s.split("\n")
boxes, targets, spaces, walls, size, darkboxes = [], [], [], [], len(s), []
for i in range(len(s)):
    for j in range(len(s[i])):
        if s[i][j] == '#':
            walls += [(j, i)]
        if s[i][j] == '*':
            targets += [(j, i)]
            spaces += [(j, i)]
        if s[i][j] == '$':
            spaces += [(j, i)]
        if s[i][j] == 'x':
            boxes += [(j, i)]
        if s[i][j] == '@':
            spaces += [(j, i)]
print ("size = ", size)
print ("walls = ", walls)
print ("targets = ", targets)
print ("boxes = ", boxes)
print ("darkboxes = ", darkboxes)
print ("spaces = ", spaces)

print ("(GameState (MapState %d %s %s %s %s %s) (%d, %d))"%(size, walls, targets, boxes, darkboxes, spaces, 0, 0))
