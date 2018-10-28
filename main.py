import numpy as np
import pandas as pd
from scipy.stats import norm, multivariate_normal, uniform

SALES = [26.91,47.79,37.43,31.24,33.8,51.03,43.72,35.2,39.27,74.47,61.17,47.53,48.05,74.78,51.19,40.4,45.51,78.29,50.76,41.03]
GENERATION = [0,4,8,12,16]

class SBass:
    def __init__(self,sales, price=None, competition=None, topics=None, heterogeneoty=True, generation=1,
                 prior=None):
        self.sales = sales
        self.price = price
        self.comp = competition
        self.topics = topics
        self.hete = heterogeneoty
        self.g  = generation
        if prior is None:
            self.pr = {"mp":[1,1],"pp":[0.5,0.1],"qq":[0.5,0.1]}
        else:
            assert isinstance(prior,dict), "prior must be a dictionary."
            assert prior.get('mp') is not None, "prior must have 'mp' key!"
            assert prior.get('pp') is not None, "prior must have 'pp' key!"
            assert prior.get('qp') is not None, "prior must have 'qp' key!"
            self.pr = prior

    def prior(self,params):
        pass

    def likelihood(self,params):
        pass

    def posterior(self,params):
        self.likelihood(params)["sumll"] + self.prior(params)





if __name__ == "__main__":
    pass




        