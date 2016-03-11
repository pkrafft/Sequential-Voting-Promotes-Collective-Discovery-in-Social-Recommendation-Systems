
library(ggplot2)

source('tools.R')

data = read.csv('../scores.csv', stringsAsFactors = F)
data[data[,'Course'] == 'ART','Course'] = 'Art'

data = data[!is.na(data[,'Change']),]

results = summarySE(data, measurevar="Change", groupvars=c("Course","Type"))

pdf('results-by-course.pdf', 8, 4)

ggplot(results, aes(x=Course, y=Change, fill=Type)) + 
    geom_bar(position=position_dodge(), stat="identity",
             colour="black", 
             size=1) +      
    geom_errorbar(aes(ymin=Change-se, ymax=Change+se),
                  size=1,   
                  width=.2,
                  position=position_dodge(.9)) +
    xlab("Course") +
    ylab("Self-Reported Learning") +
    scale_fill_hue(name="Condition", 
                   breaks=c("curated", "herding", "nonherd", "random"),
                   labels=c("Expert", "Sequential", "Independent", "Random")) +
    theme_bw(base_size = 20)

dev.off()

data = read.csv('../scores.csv', stringsAsFactors = F)
data[data[,'Course'] == 'ART','Course'] = 'Art'

results = summarySE(data, measurevar="Score", groupvars=c("Course","Type"))

pdf('score-results-by-course.pdf', 8, 4)

ggplot(results, aes(x=Course, y=Score, fill=Type)) + 
    geom_bar(position=position_dodge(), stat="identity",
             colour="black", 
             size=1) +      
    geom_errorbar(aes(ymin=Score-se, ymax=Score+se),
                  size=1,   
                  width=.2,
                  position=position_dodge(.9)) +
    xlab("Course") +
    ylab("Average Test Scores") +
    scale_fill_hue(name="Condition", 
                   breaks=c("curated", "herding", "nonherd", "random"),
                   labels=c("Expert", "Sequential", "Independent", "Random")) +
    theme_bw(base_size = 20)

dev.off()


pdf('levels-1.pdf', 4, 4)

data = read.csv('../scores.csv', stringsAsFactors = F)
data[data[,'Course'] == 'ART','Course'] = 'Art'

fit = lm(Score ~ lvlBefore, data = data)

tmp = data[!is.na(data[,'lvlBefore']),]
results = summarySE(tmp, measurevar="Score", groupvars=c("lvlBefore"))

ggplot(results, aes(x=lvlBefore, y=Score)) +
    geom_abline(intercept = fit$coef[1], slope = fit$coef[2], color = '#3366ff') + 
    geom_jitter(data=tmp, aes(x=lvlBefore, y=Score), alpha = 0.05) +
    geom_point(color = '#cc8800') +
    geom_errorbar(aes(ymin=Score-se, ymax=Score+se),
                  size=1.5,
                  width=.3,
                  position=position_dodge(.9), color = '#cc8800') +
    xlab("Self-Reported Level Before") +
    ylab("Test Score") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_bw(base_size = 18) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 

dev.off()

pdf('levels-2.pdf', 4, 4)

data = read.csv('../scores.csv', stringsAsFactors = F)
data[data[,'Course'] == 'ART','Course'] = 'Art'

fit = lm(Score ~ lvlAfter, data = data)

tmp = data[!is.na(data[,'lvlAfter']),]
results = summarySE(tmp, measurevar="Score", groupvars=c("lvlAfter"))

ggplot(results, aes(x=lvlAfter, y=Score)) +
    geom_abline(intercept = fit$coef[1], slope = fit$coef[2], color = '#3366ff') + 
    geom_jitter(data=tmp, aes(x=lvlAfter, y=Score), alpha = 0.05) +
    geom_point(color = '#cc8800') +
    geom_errorbar(aes(ymin=Score-se, ymax=Score+se),
                  size=1.5,   
                  width=.3,
                  position=position_dodge(.9), color = '#cc8800') +
    xlab("Self-Reported Level After") +
    ylab("Test Score") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_bw(base_size = 18) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 


dev.off()

#data = read.csv('../explanation-grades.csv')
data = read.csv('../explanation-gradesUPDATED.csv', stringsAsFactors = F)

norm = data[,'E']
#norm = 10

data = as.data.frame(
    rbind(
    cbind(data[,'C']/norm, 'Expert', data[,'Course']),
    cbind(data[,'R']/norm, 'Random', data[,'Course']),
    cbind(data[,'N']/norm, 'Independent', data[,'Course']),
    cbind(data[,'H1']/norm, 'Sequential', data[,'Course']),
    cbind(data[,'H2']/norm, 'Sequential', data[,'Course']),
    cbind(data[,'H3']/norm, 'Sequential', data[,'Course'])
    ))
colnames(data) = c('Grade','Type', 'Course')
data[,'Grade'] = as.numeric(as.character(data[,'Grade']))
data[,'Type'] = factor(data[,'Type'], levels = c("Expert", "Sequential", "Independent", "Random"))

results = summarySE(data, measurevar="Grade", groupvars=c("Type","Course"))

pdf('grades.pdf', 8, 4)

ggplot(results, aes(x=Course, y=Grade, fill=Type)) + 
    geom_bar(position=position_dodge(), stat="identity",
             colour="black",
             size=1) +      
    geom_errorbar(aes(ymin=Grade-se, ymax=Grade+se),
                  size=1,   
                  width=.2,
                  position=position_dodge(.9)) +
    xlab("Course") +
    ylab("Average Explanation Grades") +
    #guides(fill=FALSE) +
    scale_fill_hue(name="Experiment Condition", 
                   breaks=c("Expert", "Sequential", "Independent", "Random"),
                   labels=c("Expert", "Sequential", "Independent", "Random")) +
    theme_bw(base_size = 20)

dev.off()

pdf('inequality.pdf', 6, 4)

data = read.csv('../inequalities.csv', stringsAsFactors = F)
data[data[,'Course'] == 'ART','Course'] = 'Art'

results = summarySE(data, measurevar="Inequality", groupvars=c("Condition","Course"))

ggplot(results, aes(x=Course, y=Inequality, fill=Condition)) + 
    geom_bar(position=position_dodge(), stat="identity",
             colour="black",
             size=1) +      
    geom_errorbar(aes(ymin=Inequality-se, ymax=Inequality+se),
                  size=1,   
                  width=.2,
                  position=position_dodge(.9)) +
    xlab("Course") +
    ylab("Average Inequality") +
    scale_fill_hue(name="Condition", 
                   breaks=c("Expert", "Sequential", "Independent", "Random"),
                   labels=c("Expert", "Sequential", "Independent", "Random")) +
    theme_bw(base_size = 20)

dev.off()
