# Part b

## Method 1: highest priority person gets second choice
- Idee: hoge priority: we willen veel voor je doen
- Temporary updates
 - 5 -> 20 -> 70 -> 80 -> 5
 1) Take out 5 -> 20
 2) Add 5-> 140
 3) No cycle
 4) ...
 5) when terminated, reassign 5->20

- When broken, do we continue with 5 -> 140 or 20 70 80?
    - met 5

- If w chain, continue with next priority or same cycle but then other link?

## Method 2: lowest priority person gets second choice
- Idee: kans voor hoge priority om favoriet te krijgen is groter.
- Risico: favoriet wordt gekaapt

## Method 3: find w chain
- Stupid because waiting list sucks

### a
- Say we have 3 cycles of length > 4 and one w-chain
- Take the w chain and ...

NO

### b
- Same
- And apply method 1. If ends in w chain, take the w chain

## Method 4: Find favourite person who likes 5
- Get column index preference 5 per patient. Get lowest column index (wants it most). Check if 5 like this person "enough".

## Method 5: Take next (limit-1) people and ask for priority
- 5 -> 20 -> 70 -> 80 -> 5
- Ask 70 whether he prefers 5 over w.
    - If yes, make cycle 5 -> 20 -> 70 -> 5.
    - If not, ask 20. If yes, make cycle 5 -> 20 -> 5.
    - If not, go on with 6.

- Minpunt: wat als 5 en w allebei helemaal achteraan staan maar 5 is voor w. Stom voor 70, bv.
- Verder met 6 betekent neglecten van 5.


## Method 100: Stop market
NO