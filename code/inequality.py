
import pandas as pd
import numpy as np

def inequality(pops):
    pops = np.round(pops).astype(int)
    inds = pops > 0
    total = sum(pops)
    if not total > 0:
        return 0.0
    n_zeros = sum(~inds)
    pops = pops[inds]
    m = pops/float(total)
    s = n_zeros
    for i in range(len(m)-1):
        s += sum(np.abs(m[i] - m[(i+1):]))
    s /= float(len(m) + n_zeros - 1)
    return s

data = pd.read_csv('../total-votes.csv')

q_id = data['question']

out = []

cols = ['1N Votes', '1H Votes', '2H Votes', '3H Votes']
col_type = dict(zip(cols, ['Non-Social', 'Social', 'Social', 'Social']))

for c in cols:
    
    for t in set(data['type']):
        
        sub = data[data['type'] == t]
        ls = set(sub['lesson'])
        
        for l in ls:

            sub = data[(data['type'] == t) & (data['lesson'] == l)]
            qs = set(sub['question'])
            
            for q in qs:

                sub = data[(data['type'] == t) & (data['lesson'] == l) & (data['question'] == q)]
                
                pops = np.array(sub[c])
            
                out += [[col_type[c], t, inequality(pops)]]

out = pd.DataFrame(out, columns = ['Condition', 'Course', 'Inequality'])

out.to_csv('../inequalities.csv')
