import pprint as pp

"""
3 4
GERTIE HERBERT MIRIAM WALLACE
AL BE PE SL
1928 1929 1932 1935
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
    for j in range(i+1, num_features):
        assoc = {}
        for k in range(num_elements):
            assoc[features[i][k]] = list(features[j])
        possible.append(assoc)

#pp.pprint(possible)

"""
CONSTRAINTS

-disassociations (harry does not own the broom)
-POE (harry owns the broom so no one else can own the broom)
-links ()

"""

# go until no constraints left to apply
for c in constraints:
    for p in possible:

        # diconnect c1
        if c[0] in p and c[1] in p[c[0]]:
            p[c[0]].pop(p[c[0]].index(c[1]))
        # diconnect c0
        elif c[1] in p and c[0] in p[c[1]]:
            p[c[1]].pop(p[c[1]].index(c[0]))

pp.pprint(possible)
