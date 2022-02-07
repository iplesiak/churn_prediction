# churn_prediction
Prediction of customers' churn of a large dutch energy supplier. 

After the analysis of above mentioned models, it was concluded that the best performing method for predicting a churn 
is boosting with a hit rate 80.46%, however, the main drawback of the model is the difficulty of interpreting the results. 
Therefore, it is suggested to use the second best performing model - stepwise logistic regression with accuracy (hit rate) 75.6%, 
which is not only pretty close to boosting model, but also provides a clear interpretation possibility. Specifically, it was identified 
that variables such as Income, Relation length, followed by Email list, Province Zuid-Holland, Gas usage, Electricity usage, Home 
label and contract type are the most influential variables in predicting churn. To make a picture even more clearer an increase by 1% 
of customers that can be contacted by Email would increase churn by 80.24% and an 1% increase in customers from Zuid-Holland would 
increase churn by 36.96%. This leads to the assumption that E-Mail Marketing has a negative effect on customers' loyalty to the firm, 
while customers from Zuid Holland seem to be more explorative and price sensitive concerning their electricity contracts. Gas usage 
and contract type also have a positive impact on churn: an 1% increase in Gas usage would increase the probability of churn by 0.14% 
and an 1% increase in people having a flexible contract will lead to an increase of almost 600%. Moreover, the higher the electricity 
usage the higher the probability of churning. Home label also has a positive impact on churn, an 1% increase of customers with a Homelable G, 
the worst one, increases churn by 94.07%. The lower the energy label of a home, the higher the costs and the probability of churn.
