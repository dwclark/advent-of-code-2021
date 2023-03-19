import static Game.*

def toTest = """
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########""";

def str = """
#############
#...........#
###A#D#B#C###
  #B#C#D#A#
  #########""";

def game = new Game(str)
def solved = game.playAStar()

println "${solved}"

/*def strFinished = """#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########"""

assert new Game(strFinished).finished*/

/*def strAlmost = """
#############
#A....D.....#
###.#B#C#.###
  #A#B#C#D#
  #########"""

def almost = new Game(strAlmost);
assert !almost.unobstructed(new Point(3,3), new Point(1,1))
assert almost.unobstructed(new Point(3,3), new Point(1,2))
assert almost.unobstructed(new Point(3,3), new Point(1,5))
assert !almost.unobstructed(new Point(3,3), new Point(1,10))

assert almost.unobstructed(new Point(1,1), new Point(2,3))
assert !almost.unobstructed(new Point(1,1), new Point(3,3))
assert almost.unobstructed(new Point(3,9), new Point(2,9))
assert almost.unobstructed(new Point(3,9), new Point(1,11))
assert almost.unobstructed(new Point(3,9), new Point(1,8))
assert !almost.unobstructed(new Point(3,9), new Point(1,5))

println almost.estimatedCost

def strCantPass = """
#############
#....C......#
###A#B#.#D###
  #A#B#C#D#
  #########"""

def cantPass = new Game(strCantPass)
assert cantPass.unobstructed(new Point(2,3), new Point(1,1))
assert !cantPass.unobstructed(new Point(2,3), new Point(1,9))
assert cantPass.unobstructed(new Point(2,9), new Point(1,8))
assert !cantPass.unobstructed(new Point(2,9), new Point(1,1))
println cantPass.estimatedCost


println almost.playTree()
println cantPass.playTree()
*/
