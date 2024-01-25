# Gery Casiez

import pandas as pd
import statistics

def getData():
    df = pd.read_excel('100papersART.xlsx')

    data = []

    for index, row in df.iterrows():
        if not(pd.isna(row['field'])) and row['field'] != 'Off topic':
            data.append({
                'field': row['field'],
                'nbSubjects': row['NbSubjects'],
                'n': row['n (cell size)'],
                'design': row['Design'],
                'exptype': row['exptype'],
                'dependentMeasures': row['dependentMeasures'],
                'mainEffectART': row['main effects with ART'],
                'interactionEffectART': row['interaction effects with ART'],
                'dataAvailable': row['Data available (supplementary)?']
            })
    return data

def analyzeNbFactors(data):
    nbfactors = []
    for d in data:
        factors = str(d['design']).split("x")
        nbfactors.append(len(factors))

    nb1 = len([d for d in nbfactors if d == 1])
    nb2 = len([d for d in nbfactors if d == 2])
    nb3 = len([d for d in nbfactors if d == 3])
    return {'min': min(nbfactors),
            'max': max(nbfactors),
            'percentage1': nb1/len(nbfactors)*100,
            'percentage2': nb2/len(nbfactors)*100,
            'percentage3': nb3/len(nbfactors)*100}

def analyzeNblevels(data):
    nblevels = []
    # keep 2 factors
    filteredData = [d for d in data if len(str(d['design']).split("x")) == 2]
    for d in filteredData:
        factors = str(d['design']).split("x")
        for f in factors:
            nblevels.append(int(f))
    nb2 = len([d for d in nblevels if d == 2])
    nb3 = len([d for d in nblevels if d == 3])
    return {'min': min(nblevels),
            'max': max(nblevels),
            'percentage2': nb2/len(nblevels)*100,
            'percentage3': nb3/len(nblevels)*100}

def analyzeNbParticipants(data):
    nbPart = [d['nbSubjects'] for d in data]
    return {'min': min(nbPart),
            'max': max(nbPart),
            'med': statistics.median(nbPart)

    }

def analyzeN(data):
    nbPart = [d['n'] for d in data]
    return {'min': min(nbPart),
            'max': max(nbPart),
            'med': statistics.median(nbPart)

    }

def analyzeInteraction(data):
    studiesInteraction = [d for d in data if d['interactionEffectART'] == 'yes' or d['interactionEffectART'] == 'no']
    yes = [d for d in studiesInteraction if d['interactionEffectART'] == 'yes']
    return {'percentageyes': len(yes)/len(studiesInteraction)*100}

def analyzeDependantMeasure(data):
    ratio = [d for d in data if d['dependentMeasures'] == 'ratio']
    return len(ratio)/len(data)*100

def computeStats(data):
    stats = {}
    stats['nbPapersAnalyzed'] = len(data)
    stats['domains'] = ", ".join(sorted(list(set([d['field'] for d in data]))))
    stats['nbwithin'] = len([d for d in data if d['exptype'] == 'within'])
    stats['nbbetween'] = len([d for d in data if d['exptype'] == 'between'])
    stats['nbmixed'] = len([d for d in data if d['exptype'] == 'mixed'])
    stats['nbfactors'] = analyzeNbFactors(data)
    stats['nbLevels2factors'] = analyzeNblevels(data)
    stats['nbPart'] = analyzeNbParticipants(data)
    stats['nbN'] = analyzeN(data)
    stats['maineffect'] = len([d for d in data if d['mainEffectART'] == 'yes']) / len(data) * 100
    stats['interaction'] = analyzeInteraction(data)
    stats['dependant'] = analyzeDependantMeasure(data)
    stats['sharing'] = len([d for d in data if d['dataAvailable'] == 'yes']) / len(data) * 100
    return stats

def printText(s):
    t = s['nbPapersAnalyzed']
    txt = """To analyze how the ART is used in experimental research, we collected in November 2023 the %s most recent papers written in English and citing [@wobbrock:2011] on Google Scholar. The papers were published in %s domains. %2.1f%% of the papers reported experiments using a within-participants design, %2.1f%% a between-participants design and %2.1f%% a mixed-participants one. The number of factors ranged from %s to %s, with 2 factors representing %2.1f%% of the experiments (1: %2.1f%%, 3: %2.1f%%). For 2 factors, the number of levels ranged between %s and %s, with 2 levels being the most widely used (%2.1f%%), followed by 3 (%2.1f%%). The studies had between %d and %d participants (median = %s), with subgroups ranging from %d to %d (median = %s). For dependent variables, %2.1f%% of the studies used ratio variables and the remaining ordinal variables.  %2.1f%% of the studies found at least one significant main effect in the results, using the ART. %2.1f%% of the studies studying the interaction between factors using the ART found at least one significant interaction. Only %2.1f%% of the papers made the data of their studies publicly available."""%( 
         t, 
         s['domains'], 
         s['nbwithin']/t*100, 
         s['nbbetween']/t*100, 
         s['nbmixed']/t*100, 
         s['nbfactors']['min'], 
         s['nbfactors']['max'], 
         s['nbfactors']['percentage2'], 
         s['nbfactors']['percentage1'], 
         s['nbfactors']['percentage3'], 
         s['nbLevels2factors']['min'], 
         s['nbLevels2factors']['max'], 
         s['nbLevels2factors']['percentage2'], 
         s['nbLevels2factors']['percentage3'], 
         s['nbPart']['min'], 
         s['nbPart']['max'], 
         s['nbPart']['med'], 
         s['nbN']['min'], 
         s['nbN']['max'],
         s['nbN']['med'],
         s['dependant'],
         s['maineffect'],
         s['interaction']['percentageyes'],
         s['sharing'])

    print(txt)


data = getData()
stats = computeStats(data)
printText(stats)

