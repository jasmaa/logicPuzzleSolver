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
            p[c[0]].pop(p[c[0]].index(c[1]))
        # diconnect c0
        elif c[1] in p and c[0] in p[c[1]]:
            p[c[1]].pop(p[c[1]].index(c[0]))

    # check for POE
    for p in possible:
        for feature in p:
            if len(p[feature]) == 1:
                poe_list = list(p.keys())
                poe_list.pop(poe_list.index(feature))
                
                for f in poe_list:
                    new_c = [f, p[feature][0]]
                    if new_c not in constraint_graveyard:
                        constraints.append(new_c)
                        constraint_graveyard.append(new_c)

    # TODO implement links
                        
    #pp.pprint(possible)
    constraint_graveyard.append(constraints.pop(0))

pp.pprint(possible)
