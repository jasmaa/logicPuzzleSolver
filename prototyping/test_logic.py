import pprint as pp

"""
num_features = 3
num_elements = 4

features = [
        ['GERTIE', 'HERBERT', 'MIRIAM', 'WALLACE'],
        ['AL', 'BE', 'PE', 'SL'],
        ['1928', '1929', '1932', '1935'],
    ]

constraints = [
        ['HERBERT', 'BE'],
        
        ['SL', 'GERTIE'],
        ['SL', 'HERBERT'],
        
        ['BE', '1935'],
        
        ['1932', 'AL'],
        ['1932', 'BE'],

        ['PE', '1928'],
        ['PE', '1932'],
        ['PE', '1935'],

        ['WALLACE', 'BE'],
        ['WALLACE', 'PE'],
        ['WALLACE', 'SL'],
    ]
"""

# === read file ===
inp = []
with open("../puzzles/puzzle03.txt") as f:
    for i in f.read().split("\n"):
        inp.append(i.split(" "))
    
num_features = int(inp[0][0])
num_elements = int(inp[0][1])

features = []
for i in range(num_features):
    features.append(inp[i+1])

constraints = []
for i in range(len(inp) - num_features - 1):
    constraints.append(inp[i + num_features + 1])

constraint_graveyard = []

"""

PERSON, PLACE, YEAR

PERSON->PLACE
PERSON->YEAR
PLACE->YEAR
"""

possible = []

"""
loop thru constraints
pop off diconnects (bidirectional popping???)
go until 1 possibility for all
"""

# build list of possibilities
for i in range(num_features):
    for j in range(num_features):
        if i != j:
            assoc = {}
            for k in range(num_elements):
                assoc[features[i][k]] = set(features[j])
            possible.append(assoc)

#pp.pprint(possible)

"""
CONSTRAINTS

-disassociations (harry does not own the broom)
-POE (harry owns the broom so no one else can own the broom)
-links (harry is definitely associated with broom,
        broom is not associated with 42 or 39,
        therefore, harry is not associated with 42 or 39)

"""

# go until no constraints left to apply
while len(constraints) > 0:

    c = constraints[0]

    # loop for disassociations
    for p in possible:
        # disconnect c1
        if c[0] in p and c[1] in p[c[0]]:
            p[c[0]].remove(c[1])
        # diconnect c0
        elif c[1] in p and c[0] in p[c[1]]:
            p[c[1]].remove(c[0])
            

    # check for POE
    for p in possible:
        for feature in p:
            if len(p[feature]) == 1:
                for f in p:
                    if f != feature:
                        p[f] -= p[feature]

    """
    Find a definite association
    go thru possible with both elements
        if elements is a key, find intersection of lists
        if elements is in the bucket, disassociate element with keys that don't contain element
    """

    """
    # TODO implement links?
    for p in possible:
        for feature in p:
            # Detect definite
            if len(p[feature]) == 1:
                element = p[feature][0]
                print(element)

                # go thru with 1st part
                for i in possible:
                    # element is key
                    if feature in i.keys():
                        # disassociate
                        for f in features:
                            constraints.append([element, ])

                    
                    # element is in bucket
                    for j in i:
                        if feature in i[j]:
                    """   
     
    #pp.pprint(possible)
    constraint_graveyard.append(constraints.pop(0))

pp.pprint(possible)
