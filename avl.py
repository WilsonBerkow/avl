class AVLTree:
    """AVLTree which can be used as a set or map"""
    def __init__(self, v, left=None, right=None):
        self.value = v
        self.assoc = None
        self.left = left
        self.right = right
        self._recalc_height()

    def search(self, v):
        """
        Returns path to v (as a list of AVLTrees), if it is in self. Otherwise returns
        path to where to insert it, followed by None.
        """
        path = [self]
        cursor = self
        while cursor is not None and cursor.value != v:
            if v > cursor.value:
                cursor = cursor.right
            else:
                cursor = cursor.left
            path.append(cursor)
        return path

    def lookup(self, v):
        path = self.search(v)
        if path[-1] is None:
            raise KeyError("key not found in AVLTree")
        return path[-1].assoc

    def assign(self, v, assoc):
        path = self.search(v)
        if path[-1] is not None:
            path[-1].assoc = assoc
            return # it is already in the AVL tree
        tip = path[-2]
        if v > tip.value:
            assert tip.right is None
            tip.right = AVLTree(v)
            tip.right.assoc = assoc
        else:
            assert tip.left is None
            tip.left = AVLTree(v)
            tip.left.assoc = assoc
        for t in reversed(path[:-1]):
            t._recalc_height()
            # after first balancing this will become a slow no-op
            if t.balance_factor() not in [-1, 0, 1]:
                t.balance()

    def insert(self, v):
        return self.assign(v, None)

    def _recalc_height(self):
        left = -1 if self.left is None else self.left.height
        right = -1 if self.right is None else self.right.height
        self.height = 1 + max(left, right)

    def balance_factor(self):
        return height(self.right) - height(self.left)

    def right_rotate(self):
        left, value, assoc, right = self.left, self.value, self.assoc, self.right
        self.value = left.value
        self.assoc = left.assoc
        self.left = left.left
        self.right = AVLTree(value, left.right, right)
        self.right.assoc = assoc

    def left_rotate(self):
        left, value, assoc, right = self.left, self.value, self.assoc, self.right
        self.value = right.value
        self.assoc = right.assoc
        self.right = right.right
        self.left = AVLTree(value, left, right.left)
        self.left.assoc = assoc

    def balance(self):
        bf = self.balance_factor()
        if bf in [-1, 0, 1]:
            return
        assert bf in [-2, 2]
        if bf == -2:
            if self.left.balance_factor() < 0:
                # outside-insertion
                self.right_rotate()
            else:
                assert self.left.balance_factor() > 0
                # inside-insertion
                self.left.left_rotate()
                self.right_rotate()
        else:
            if self.right.balance_factor() > 0:
                # outside-insertion
                self.left_rotate()
            else:
                assert self.right.balance_factor() > 0
                # inside-insertion
                self.right.right_rotate()
                self.left_rotate()
        self.height = 1 + max(self.left.height, self.right.height)

    def __repr__(self):
        if self.right is None and self.left is None:
            return "AVLTree({!r})".format(self.value, self.height)
        h = "AVLTree({!r}, # height: {}\n".format(self.value, self.height)
        if self.right is None:
            s = "{!r})".format(self.left)
        else:
            s = "{!r},\n{!r})".format(self.left, self.right)
        return h + indent(s, 2)

    def __str__(self):
        s = ""
        if self.left is not None:
            s = str(self.left)
        s += str(self.value)
        if self.assoc is not None:
            s += ":" + str(self.assoc)
        s += ","
        if self.right is not None:
            s += "," + str(self.right)
        return s

    def from_list(lst):
        if lst == []:
            return None
        t = AVLTree(lst[0])
        for v in lst[1:]:
            t.insert(v)
        return t

    def from_dict(d):
        if d == {}:
            return None
        t = None
        for k, v in d.items():
            if t is None:
                t = AVLTree(k)
            t.assign(k, v)
        return t

def indent(s, n):
    return '\n'.join([ ' ' * n + line for line in s.split('\n') ])

def height(t):
    if t is None:
        return -1
    return t.height

def balance_factor(t):
    return height(t.right) - height(t.left)

t = AVLTree(20,
        AVLTree(10, AVLTree(5), None),
        AVLTree(30, AVLTree(25),
            AVLTree(40, AVLTree(35), AVLTree(45))))

def rot13():
    d = {}
    for i in range(26):
        d[chr(i + ord('A'))] = chr(ord('A') + (i + 13) % 26)
    return d

def rot13_tree():
    return AVLTree.from_dict(rot13())
