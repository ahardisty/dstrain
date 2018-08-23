p <- ggplot(data = customer_metrics[,.N, by = .(day, over_under, month)][order(over_under)]
       , aes(x = factor(day), y = N, fill = over_under)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~month ) +
  scale_fill_manual(values = c(CP$blue, CP$green, CP$red),
                    'Model Performance',
                    labels = c('Temp better than constant'
                               , 'Temp even with constant'
                               , 'Temp worse than constant')) +
  geom_hline(data = melt(customer_metrics[,.(sample75 = uniqueN(customer)*.75
                                             , sample50 = uniqueN(customer)*.50
                                             , sample25 = uniqueN(customer)*.25), by = .(month)
                                          ],id.vars = 'month',variable.name = 'line',value.name = 'N')
             ,aes(yintercept = N, linetype = line)
             , color = 'black', size = 1.5) +

p + geom_text(aes(0,2000 ), label = 'blue')
  
  p + scale_linetype_discrete(name = 'Expected Breaks'
                          , labels = c('.75 of sample: 2145'
                                       , '.50 of sample: 1430'
                                       , '.25 of sample: 715')) 

