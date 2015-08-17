# CAnova
Class to perform anova and plot results visually

# Constructor - usage
Call constructor CAnova with data vector (numeric values) and grouping factor (factor variable)

# functions
# plot.canova
plots 4 different plots, showing the model diagnostics. The details of the plots can be read here: 
http://www.evernote.com/l/ASCULReo83dEcYSpxcyMiMvTw9pMMWJcWSA/
The first plot shows the grand mean, group means, residuals in each group, and model coefficients i.e. regression coefficients which
are the differences in means for anova.

The second plot shows the SSMt (total sum of squares) for the null model, i.e. when no grouping done.
Third plot shows SSMm (Sum of squares for model) i.e. predicted values (group means) and grand mean differences
Fourth plot shows SSMr (Sum of squares for residuals) i.e. unexplained error for other reasons not accounted for by model.

# print.canova
Prints various model parameters.