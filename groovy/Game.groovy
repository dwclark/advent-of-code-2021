import java.io.*
import groovy.transform.Immutable

class Game implements Comparable<Game> {

    @Immutable
    static class Point {
        int row, column

        int distance(Point rhs) {
            return Math.abs(row - rhs.row) + Math.abs(column - rhs.column)
        }
    }

    static class Pod {
        String id
        int moves
        
        Pod(String id, int moves) {
            this.id = id;
            this.moves = moves
        }

        int getCost() {
            if(id == 'A') return 1
            else if(id == 'B') return 10
            else if(id == 'C') return 100
            else if(id == 'D') return 1_000
        }

        Pod moved() { return new Pod(id, moves+1) }

        boolean isMovable() { return moves < 2 }

        @Override String toString() { id; }
        
        @Override boolean equals(Object o) {
            if(this.is(o)) return true;
            if(!(o instanceof Pod)) return false;
            return id == o.id
        }

        @Override int hashCode() { id.hashCode() }
    }
    
    public static Point pt(int row, int column) { new Point(row, column) }

    static int NUM_ROWS
    static int NUM_COLS
    static Set<Point> POSITIONS
    static final String HALL = '.'
    static int counter = 0;
    static final POD = [ 'A', 'B', 'C', 'D' ] as Set
    static final LEGAL_FIRST = [ pt(1,1), pt(1,2), pt(1,4), pt(1,6), pt(1,8), pt(1,10), pt(1,11) ] as Set;
    static final LEGAL_SECOND = [(pt(2,3)): 'A', (pt(3,3)): 'A',
                                 (pt(2,5)): 'B', (pt(3,5)): 'B',
                                 (pt(2,7)): 'C', (pt(3,7)): 'C',
                                 (pt(2,9)): 'D', (pt(3,9)): 'D' ]
    static final FINAL = [A: [pt(2,3), pt(3,3)],
                          B: [pt(2,5), pt(3,5)],
                          C: [pt(2,7), pt(3,7)],
                          D: [pt(2,9), pt(3,9)]].asImmutable()
    
    final Map<Point,Pod> positions;
    final creation;
    final energy;
    final estimatedRemaining;
    
    Game(String s) {
        creation = ++counter;
        energy = 0
        
        def reader = new StringReader(s.trim())
        def grid = reader.readLines().collect { it as List }
        NUM_ROWS = grid.size()
        NUM_COLS = grid[0].size()
        POSITIONS = [] as Set

        def tmp = [:]
        grid.eachWithIndex { row, rowIndex ->
            row.eachWithIndex { c, colIndex ->
                def p = pt(rowIndex, colIndex)
                if(POD.contains(c)) {
                    tmp[p] = new Pod(c, 0)
                    POSITIONS.add(p)
                }
                else if(c == '.') {
                    POSITIONS.add(p)
                } } }

        positions = tmp.asImmutable();
        estimatedRemaining = estimateRemaining()
    }
    
    Game(Game prev, Point from, Point to) {
        creation = ++counter
        def pod = prev.positions[from]
        energy = prev.energy + (pod.cost * from.distance(to))
        
        def tmp = new HashMap(prev.positions)
        tmp.remove(from)
        tmp[to] = pod.moved()
        positions = tmp.asImmutable()
        estimatedRemaining = estimateRemaining()
    }

    Object getAt(Point pt) { positions[pt] ?: HALL }

    int minDistance(Pod pod, List one, List two) {
        int dist = Math.min(one[0].distance(two[1]) + one[1].distance(two[0]),
                            one[0].distance(two[0]) + one[1].distance(two[1]))
        return pod.cost * dist
    }

    final int estimateRemaining() {
        int ret = 0
        def tmp = positions.inject([:]) { map, key, pod ->
            map.get(pod, []).add(key);
            map }
        
        tmp.each { pod, list ->
            ret += minDistance(pod, list, FINAL[pod.id]) }

        return ret;
    }
    
    @Override String toString() {
        def sb = new StringBuffer("Energy: ${energy}\n")
        (0..<NUM_ROWS).each { row ->
            (0..<NUM_COLS).each { col ->
                def pt = new Point(row, col)
                if(POSITIONS.contains(pt)) {
                    sb.append(this[pt])
                }
                else {
                    sb.append('#')
                } }
            sb.append("\n") }

        return sb.toString()
    }

    @Override boolean equals(Object o) {
        if(this.is(o)) return true
        if(!(o instanceof Game)) return false
        return positions.equals(o.positions)
    }

    @Override int hashCode() {
        return positions.hashCode()
    }

    boolean isFinished() {
        positions.every { pt, pod -> pod.id == LEGAL_SECOND[pt] }
    }
    
    int compareTo(Game rhs) {
        int cmp = Integer.compare(energy, rhs.energy)
        if(cmp != 0) return cmp
        else return Integer.compare(creation, rhs.creation)
    }

    int getEstimatedCost() {
        return energy + estimatedRemaining
    }

    boolean unobstructed(Point from, Point to) {
        int targetColumn = (from.row == 1) ? to.column : from.column;
        Pod pod = this[from]
        def clear = { row, col ->
            def test = this[pt(row, col)]
            return pod.is(test) || test == '.' }
        
        return (((from.row)..(to.row)).every { row -> clear(row, targetColumn) } &&
                ((from.column)..(to.column)).every { column -> clear(1, column) })
    }
    
    List<Game> possible() {
        def ret = []
        positions.each { from, pod ->
            if(from.row == 3 &&
               LEGAL_SECOND.containsKey(from) &&
               LEGAL_SECOND[from] == pod.id) {
                return
            }

            if(from.row == 2 &&
               LEGAL_SECOND.containsKey(from) &&
               LEGAL_SECOND[from] == pod.id &&
               positions[pt(3, from.column)] == pod.id) {
                return
            }
            
            if(pod.movable) {
                if(from.row == 1) {
                    LEGAL_SECOND.each { to, s ->
                        if(pod.id == s && unobstructed(from, to)) {
                            ret.add(new Game(this, from, to))
                        }
                    }
                }
                else {
                    LEGAL_FIRST.each { to ->
                        if(unobstructed(from, to)) {
                            ret.add(new Game(this, from, to));
                        }
                    }
                }
            }
        }
        
        return ret;
    }

    Game nextGame(Set<Game> visited, SortedMap<Integer,List<Game>> tree) {
        def entryIter = tree.entrySet().iterator()
        while(entryIter.hasNext()) {
            def entry = entryIter.next()
            def enery = entry.key
            def list = entry.value
            def listIter = list.iterator()
            while(listIter.hasNext()) {
                Game game = listIter.next()
                if(!visited.contains(game)) {
                    listIter.remove()
                    visited.add(game)
                    return game;
                }
            }
            
            entryIter.remove();
        }

        return null;
    }

    void addGame(Set<Game> visited, SortedMap<Integer,List<Game>> tree, Game game) {
        if(visited.contains(game)) {
            return
        }

        List<Game> list = tree[game.energy]
        if(list == null) {
            list = new ArrayList();
            tree[game.energy] = list
        }

        list.add(game)
    }
    
    Game playQueue() {
        def queue = new PriorityQueue()
        def visited = new HashSet()
        
        queue.add(this)
        
        def next
        while((next = queue.poll()) != null) {
            if(next.finished) {
                return next
            }

            visited.add(next)
            next.possible().each { newGame ->
                if(!visited.contains(newGame)) {
                    queue.add(newGame)
                }
            }
        }
        
        return null
    }

    //much faster than priority queue version
    Game playTree() {
        def tree = new TreeMap()
        def visited = new HashSet()
        
        addGame(visited, tree, this)
        
        def next
        while((next = nextGame(visited, tree)) != null) {
            if(next.finished) {
                return next
            }

            println "Energy: ${next.energy}"
            visited.add(next)
            next.possible().each { newGame -> addGame(visited, tree, newGame) }
        }
        
        return null
    }

    static int compareAStar(Game lhs, Game rhs) {
        int cmp = Integer.compare(lhs.estimatedCost, rhs.estimatedCost)
        if(cmp != 0) return cmp
        else return Integer.compare(lhs.creation, rhs.creation)
    }

    Game playAStar() {
        def queue = new PriorityQueue(128, Game.&compareAStar)
        def visited = new HashSet()
        
        queue.add(this)
        
        def next
        while((next = queue.poll()) != null) {
            if(next.finished) {
                return next
            }

            visited.add(next)
            next.possible().each { newGame ->
                if(!visited.contains(newGame)) {
                    queue.add(newGame)
                }
            }
        }
        
        return null
    }
}
