

data = read.csv('../ALLanswers-completed.csv', stringsAsFactors = F)
data[data[,'Answer'] == 'True','Answer'] = 1
data[data[,'Answer'] == 'False','Answer'] = 0
data[,'Answer'] = as.numeric(data[,'Answer'])

data[data[,'lvlAfter'] < 0,'lvlAfter'] = NaN
data[data[,'lvlBefore'] < 0,'lvlBefore'] = NaN

data[,'cs'] = data[,'Course'] == 'CS'
data[,'art'] = data[,'Course'] == 'ART'

data[,'score'] = data[,'Answer']
data[data[,'cs'],'score'] = data[data[,'cs'],'score']/5
data[data[,'art'],'score'] = data[data[,'art'],'score']/20

data[,'change'] = data[,'lvlAfter'] - data[,'lvlBefore']

score = aggregate(data[,'score'],list(data[,'session__sessionId']),sum)[2]
change = aggregate(data[,'change'],list(data[,'session__sessionId']),unique)[2]
experiment = aggregate(data[,'Experiment'],list(data[,'session__sessionId']),unique)[2]
type = aggregate(data[,'Type'],list(data[,'session__sessionId']),unique)[2]
course = aggregate(data[,'Course'],list(data[,'session__sessionId']),unique)[2]
lB = aggregate(data[,'lvlBefore'],list(data[,'session__sessionId']),unique)[2]
lA = aggregate(data[,'lvlAfter'],list(data[,'session__sessionId']),unique)[2]

data = data.frame(list(change, score, experiment, type, course, lB, lA))
colnames(data) = c('Change', 'Score', 'Experiment', 'Type', 'Course', 'lvlBefore', 'lvlAfter')

data[,'herding'] = data[,'Type'] == 'herding'
data[,'nonherd'] = data[,'Type'] == 'nonherd'
data[,'random'] = data[,'Type'] == 'random'
data[,'curated'] = data[,'Type'] == 'curated'

data[,'herd1'] = data[,'Experiment'] == 'herd-1'
data[,'herd2'] = data[,'Experiment'] == 'herd-2'
data[,'herd3'] = data[,'Experiment'] == 'herd-3'
data[,'artherd1'] = data[,'Experiment'] == 'artherd-1'
data[,'artherd2'] = data[,'Experiment'] == 'artherd-2'
data[,'artherd3'] = data[,'Experiment'] == 'artherd-3'

data[,'cs'] = data[,'Course'] == 'CS'
data[,'art'] = data[,'Course'] == 'ART'

d = data[data[,'cs'],]
t.test(d[d[,'herding'],'Change'], d[d[,'nonherd'],'Change'])
t.test(d[d[,'herding'],'Change'], d[d[,'random'],'Change'])
t.test(d[d[,'herding'],'Change'], d[d[,'curated'],'Change'])

d = data[data[,'art'],]
t.test(d[d[,'herding'],'Change'], d[d[,'nonherd'],'Change'])
t.test(d[d[,'herding'],'Change'], d[d[,'random'],'Change'])
t.test(d[d[,'herding'],'Change'], d[d[,'curated'],'Change'])

t.test(data[data[,'herding'],'Change'], data[data[,'nonherd'],'Change'])
t.test(data[data[,'herding'],'Change'], data[data[,'random'],'Change'])
t.test(data[data[,'herding'],'Change'], data[data[,'curated'],'Change'])

library(lmerTest)

fit = lmer(Change ~ herding + cs + (1|herd1) + (1|herd2) + (1|herd3) + (1|artherd1) + (1|artherd2) + (1|artherd3), 
    data = data,
    subset = herding | nonherd)
summary(fit)

fit = lmer(Change ~ herding + cs + (1|herd1) + (1|herd2) + (1|herd3) + (1|artherd1) + (1|artherd2) + (1|artherd3), 
    data = data,
    subset = herding | random)
summary(fit)

fit = lmer(Change ~ herding + cs + (1|herd1) + (1|herd2) + (1|herd3) + (1|artherd1) + (1|artherd2) + (1|artherd3), 
    data = data,
    subset = herding | curated)
summary(fit)

fit = lmer(Change ~ herding*cs + (1|herd1) + (1|herd2) + (1|herd3) + (1|artherd1) + (1|artherd2) + (1|artherd3), 
    data = data,
    subset = herding | nonherd)
summary(fit)

fit = lmer(Change ~ herding*cs + (1|herd1) + (1|herd2) + (1|herd3) + (1|artherd1) + (1|artherd2) + (1|artherd3), 
    data = data,
    subset = herding | random)
summary(fit)

fit = lmer(Change ~ herding*cs + (1|herd1) + (1|herd2) + (1|herd3) + (1|artherd1) + (1|artherd2) + (1|artherd3), 
    data = data,
    subset = herding | curated)
summary(fit)




d = data[data[,'cs'],]
t.test(d[d[,'herding'],'Score'], d[d[,'nonherd'],'Score'])
t.test(d[d[,'herding'],'Score'], d[d[,'random'],'Score'])
t.test(d[d[,'herding'],'Score'], d[d[,'curated'],'Score'])

d = data[data[,'art'],]
t.test(d[d[,'herding'],'Score'], d[d[,'nonherd'],'Score'])
t.test(d[d[,'herding'],'Score'], d[d[,'random'],'Score'])
t.test(d[d[,'herding'],'Score'], d[d[,'curated'],'Score'])

t.test(data[data[,'herding'],'Score'], data[data[,'nonherd'],'Score'])
t.test(data[data[,'herding'],'Score'], data[data[,'random'],'Score'])
t.test(data[data[,'herding'],'Score'], data[data[,'curated'],'Score'])

library(lmerTest)

fit = lmer(Score ~ herding + cs + (1|herd1) + (1|herd2) + (1|herd3) + (1|artherd1) + (1|artherd2) + (1|artherd3), 
    data = data,
    subset = herding | nonherd)
summary(fit)

fit = lmer(Score ~ herding + cs + (1|herd1) + (1|herd2) + (1|herd3) + (1|artherd1) + (1|artherd2) + (1|artherd3), 
    data = data,
    subset = herding | random)
summary(fit)

fit = lmer(Score ~ herding + cs + (1|herd1) + (1|herd2) + (1|herd3) + (1|artherd1) + (1|artherd2) + (1|artherd3), 
    data = data,
    subset = herding | curated)
summary(fit)

fit = lmer(Score ~ herding*cs + (1|herd1) + (1|herd2) + (1|herd3) + (1|artherd1) + (1|artherd2) + (1|artherd3), 
    data = data,
    subset = herding | nonherd)
summary(fit)

fit = lmer(Score ~ herding*cs + (1|herd1) + (1|herd2) + (1|herd3) + (1|artherd1) + (1|artherd2) + (1|artherd3), 
    data = data,
    subset = herding | random)
summary(fit)

fit = lmer(Score ~ herding*cs + (1|herd1) + (1|herd2) + (1|herd3) + (1|artherd1) + (1|artherd2) + (1|artherd3), 
    data = data,
    subset = herding | curated)
summary(fit)

write.csv(data, file = '../scores.csv', quote = F, row.names = F)

data = read.csv('../explanation-gradesUPDATED.csv', stringsAsFactors = F)

norm = data[,'E']
#norm = 10

data = as.data.frame(
    rbind(
    cbind(data[,'C']/norm, 'Curated', data[,'Course']),
    cbind(data[,'R']/norm, 'Random', data[,'Course']),
    cbind(data[,'N']/norm, 'Non-Social', data[,'Course']),
    cbind(data[,'H1']/norm, 'Social', data[,'Course']),
    cbind(data[,'H2']/norm, 'Social', data[,'Course']),
    cbind(data[,'H3']/norm, 'Social', data[,'Course'])
    ))
colnames(data) = c('Grade','Type', 'Course')
data[,'Grade'] = as.numeric(as.character(data[,'Grade']))
data[,'Type'] = factor(data[,'Type'], levels = c("Curated", "Social", "Non-Social", "Random"))

d = data[data[,'Course'] == 'Art',]
t.test(d[d[,'Type'] == 'Curated','Grade'], d[d[,'Type'] == 'Social','Grade'])
d = data[data[,'Course'] == 'CS',]
t.test(d[d[,'Type'] == 'Curated','Grade'], d[d[,'Type'] == 'Social','Grade'])


fit = lm(Grade ~ Type + Course, data = data, subset = Type == 'Curated' |  Type == 'Social')
summary(fit)



data = read.csv('../inequalities.csv', stringsAsFactors = F)
data[data[,'Course'] == 'ART','Course'] = 'Art'
d = data[data[,'Course'] == 'Art',]
t.test(d[d[,'Condition'] == 'Social','Inequality'], d[d[,'Condition'] == 'Non-Social','Inequality'])
d = data[data[,'Course'] == 'CS',]
t.test(d[d[,'Condition'] == 'Social','Inequality'], d[d[,'Condition'] == 'Non-Social','Inequality'])


