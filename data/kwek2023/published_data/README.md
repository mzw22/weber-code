Colour manipulation type:
Type 1 - Males with the largest red coverage (%) and lowest pattern contrast
Type 2 - Males with larger red coverage (%) than blue coverage (%) and relatively high pattern contrast  
Type 3 - Males with similar red and blue coverages (%) and highest pattern contrast
Type 4 - Males with larger blue coverage (%) than red coverage (%) and relatively high pattern contrast  
Type 5 - Males with the largest blue coverage (%) and lowest pattern contrast

1. FMC - Contrast_final.csv = R codes to determine effects of proportional and absolute difference in male pattern contrast on female mate choice

Input columns:
trialno = number of trials
FemaleID = Female spider identity
Male1ID = Male spider identity of one male in the pair
Male1type = Colour manipulation type for first male 
Male1contrast = Male with higher manipulated pattern contrast 
Male2ID = Male spider identity of the second male in the pair
Male2type = Colour manipulation type for second male 
Male2contrast = Male with lower manipulated pattern contrast (i.e., base intensity)
Absdiffcont = Absolute difference in pattern contrast between two males
propdiffcont = Proportional difference in pattern contrast between two males 
log_propdiffcont = Log transformed proportional difference in pattern contrast between two males
Male1attention = Female attention time for first male in a pair (s)
Male2attention = Female attention time for second male in a pair (s)
Preferredattention = Female attention time for the preferred male (s)
Totalattention = Total female attention time for each trial (s)
Preferredmale = the male preferred by the female for each trial 
Contchoice = Female choice for the focal male (preferred = 1; not preferred = 0)
		
2. Discrimination performance - contrast.csv = R codes to determine female preference for pattern contrast for each colour pattern manipulation combination

Input columns:
Type = Colour pattern manipulation combination
Trials = Total number of trials 
Lowcontchoice = Total number of trials with female preferring low pattern contrast males
Highcontchoice = Total number of trials with female preferring high pattern contrast males
Prop = Proportion of males with low pattern contrast preferred
		
3. Discrimination performance - coverage.csv = R codes to determine female preference for each colour pattern manipulation combination

Input columns:
Type = Colour pattern manipulation combination
Trials = Total number of trials 
Redder = Total number of trials with female preferring males with larger red coverage
LessRed = Total number of trials with female preferring males with lower red coverage
Redprop = Proportion of males with high red coverage preferred

4. Discrimination performance - contrast - Figure.csv = R codes to create bargraphs to show comparisons between the proportion of females preferring low contrast and high contrast males

Input columns:
Contrast = Male pattern contrast (i.e.,high or low)
Type = Type of pattern combinations
Proportion = Proportion of females preference for males with high or low pattern contrast
samples = number of spiders
