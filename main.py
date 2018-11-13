import numpy as np
import pandas as pd
from scipy.stats import norm, multivariate_normal, uniform  ## (x, loc, scale)
import logging
from Utils.Utils import load_object, save_object
from collections import defaultdict
import matplotlib.pyplot as plt
import copy
import time

SALES = [0, 0.27, 1.12, 2.32, 1.7, 0.72, 6.89, 4.36, 3.79, 5.21, 7.37, 8.74, 8.75, 8.4, 14.1, 16.24, 18.65, 20.34, 17.07, 37.04, 35.06, 26.03, 26.91,47.79,37.43,31.24,33.8,51.03,43.72,35.2,39.27,74.47,61.17,47.53,48.05,74.78,51.19,40.4,45.51,78.29,50.76, 41.03]
#SALES = [0, 26.91,47.79,37.43,31.24,33.8,51.03,43.72,35.2,39.27,74.47,61.17,47.53,48.05,74.78,51.19,40.4,45.51,78.29,50.76, 41.03]



#print(len(SALES))

GENERATION = [0, 5, 9, 13, 17, 21, 25, 29, 33, 37]
#GENERATION = [0, 4, 8, 12, 16]

## One Generation
#SALES1 = [0.1, 1.3, 2.0, 2.91,4.79, 3.43, 2.9]
#SALES2 = [0.000, 0.844, 1.108, 1.247, 2.754, 2.863, 2.784, 2.557, 2.220, 1.824]
#GENERATION1 = [0,3]

L = len(GENERATION)
M = [1,100]
P = [0.01,100]
Q = [0.01,100]
SD = [1, 1]
MG, PG, QG = [], [], []

# Marketing Mix
ALPHA = [0.01, 100]
BETA = [0.01, 100]
ITA = [0.01, 100]
OMEGA = [0.01, 100]

ALPHAG, BETAG, ITAG, OMEGAG = [], [], [], []
#PG = P
for i in range(L):
    MG.append(M)
    QG.append(Q)
    PG.append(P)

for i in range(L):
    ALPHAG.append(ALPHA)
    BETAG.append(BETA)
    ITAG.append(ITA)
    OMEGAG.append(OMEGA)
PRIOR = {"mp":MG,"pp":PG,"qp":QG,'sd':SD,"a":ALPHAG,"b":BETAG,"I":ITAG,"o":OMEGAG}
SHOCK = {"m":0.1, "p":0.1,"q":0.1,"sd":0.1, "mm":0.1}




class SBass:
    def __init__(self,sales, price=None, competition=None, topics=None, heterogeneoty=True, generations=[0],
                 prior=None,shock=None, burn = 0, ite = 1000, log_interval= 1000, fixp = True, mmpar=None,
                ad = False):
        self.sales = np.array(sales)
        self.price = price
        self.comp = competition
        self.topics = topics
        self.hete = heterogeneoty
        self.gs  = generations
        self.t = len(self.sales)
        self.l = len(self.gs)
        self.shock = shock
        self.iter = ite
        self.burn = burn
        self.logint = log_interval
        self.fixp = fixp

        self.mmpar = mmpar

        logging.basicConfig(filename="Model_Processing.log", level=logging.NOTSET, format='%(asctime)s %(message)s',filemode='w')

        if prior is None:
            self.pr = {"mp":[[1,1]],"pp":[0.5,0.1],"qp":[[0.5,0.1]]}
        else:
            assert isinstance(prior,dict), "prior must be a dictionary."
            assert prior.get('mp') is not None, "prior must have 'mp' key!"
            assert prior.get('pp') is not None, "prior must have 'pp' key!"
            assert prior.get('qp') is not None, "prior must have 'qp' key!"
            self.pr = prior
        #
        self.m = []
        if self.fixp:
            self.p = [self.pr["pp"][0]]
        else:
            self.p = []
        self.q = []
        for i in range(self.l):
            self.m.append(self.pr["mp"][i][0])
            self.q.append(self.pr["qp"][i][0])
            if self.fixp is False:
                self.p.append(self.pr["pp"][i][0])
        self.gts = []
        self.xgs = []
        self.save = dict()
        self.save["m"] = []
        if self.fixp:
            self.save["p"] = [np.array(self.pr["pp"][0][0])]
        else:
            self.save["p"] = []
        self.save["q"] = []
        self.save["sd"] = [10]
        for i in range(self.l):
            self.gts.append(range(self.gs[i],self.t))
            self.xgs.append(list(range(self.t - self.gs[i])))
            self.save["m"].append(np.array(self.pr["mp"][i][0]))
            self.save["q"].append(np.array(self.pr["qp"][i][0]))
            if self.fixp is False:
                self.save["p"].append(np.array(self.pr["pp"][i][0]))
        self.new = copy.deepcopy(self.save)



    def prior(self,params):
        mp = np.array([0.] * self.l)
        qp = mp.copy()
        if self.fixp:
            pp = np.array([0.])
        else:
            pp = mp.copy()
        sd = np.array([0.])
        for i in range(self.l):
            mp[i] = norm.logpdf(params['m'][i], self.pr['mp'][i][0],self.pr['mp'][i][1])
            #qp[i] = norm.logpdf(params['q'][i], self.pr['qp'][i][0],self.pr['qp'][i][1])
            qp[i] = uniform.logpdf(params['q'][i], 0, 10)
            if self.fixp is False:
                pp[i] =  uniform.logpdf(params['p'][i], 0, 10)
        #pp[0] = norm.logpdf(params['p'][0], self.pr['pp'][0], self.pr['pp'][1])
        if self.fixp:
            pp[0] = uniform.logpdf(params["p"][0], 0, 5)
        sd[0] = norm.logpdf(params['sd'][0], self.pr['sd'][0], self.pr['sd'][1])
        sumll = np.sum(mp) + np.sum(qp) + np.sum(pp) + sd

        ##  Prior For price
        if self.price is not None:
            if self.hete:
                ap = norm.logpdf(params["a"][0],self.pr["a"][0],self.pr["a"][1])
            else:
                ap = 0
                for i in params["a"]:
                    ap +=  norm.logpdf(params["a"][i],self.pr["a"][0],self.pr["a"][1])
            sumll += ap

        return sumll[0]

    def likelihood(self,params,xgs):
        pred = np.sum(self.bass_pred(xgs=xgs, params = params), axis=0 )
        #print("pred:",pred)
        likelihoods = norm.logpdf(pred, self.sales,params["sd"][0] / 20)
        sumll = np.sum(likelihoods)
        return {"sumll":sumll, "pred":pred}

    def posterior(self,params,xgs):
        return self.likelihood(params,xgs)["sumll"] + self.prior(params)


    def proposal(self, coefs= None, norm=True):
        if norm:
            new = norm.rvs(self.param[0],self.param[1])
        else:
            pass
        return new

    def bf(self, p, q, xg):
        xg = np.array(xg)
        f = (1 - np.exp(- (p + q) * xg)) / (1 + (q/p) * np.exp(- (p+ q)* xg ))
        return f
    def sf(self,bigf, t):
        if t == 1:
            s = bigf[t]
        else:
            s = bigf[t] - bigf[t-1]
        return s


    def bass_pred(self, params, xgs, t= None, rleap=False):
        if t is None:
            t = self.t
        f = np.array([0.]* t * self.l ).reshape((self.l, t))
        sf = np.array([0.] * t * self.l).reshape((self.l, t))
        leap = sf.copy()
        pred = np.array([0.]* t * self.l ).reshape((self.l, t))
        bigm = np.array([0.]* t)
        for i in range(self.l):
            l = len(self.xgs[i])
            s = self.t - l
            if self.fixp:
                pi = 0
            else:
                pi = i
            f[i,s:] = self.bf(params["p"][pi], params["q"][i],  xgs[i])
            sf[i,:] = np.concatenate([ f[i,0:1], np.diff(f[i,:])],axis=0 )

            if i == 0:
                save = np.array(params["m"][i] ,dtype=np.float32)
            else:
                save = params["m"][i-1] * f[i-1,:] + params["m"][i]

            if i != (self.l - 1):
                if i != 0:
                    leap[i,:] = params["m"][i-1] * sf[i-1,:] * f[i,:]
                pred[i,:] = (save * sf[i,:] + leap[i,:]) * (1 - f[i+1,:])
            else:
                leap[i, :] = params["m"][i - 1] * sf[i - 1, :] * f[i, :]
                pred[i,:] = save * sf[i,:] + leap[i,:]
        if rleap:
            return leap
        else:
            return pred

    def MCMC(self):
        chain = dict()
        chain["m"] = np.array([0.]* self.iter * self.l).reshape((self.l,self.iter))
        chain["q"] = chain["m"].copy()
        if self.fixp:
            chain["p"] = np.array([0.] * self.iter).reshape((1,self.iter))
        else:
            chain["p"] = chain["m"].copy()
        chain["sd"] = chain["p"].copy()
        chain["pos"] = chain["p"].copy()
        chain["lh"] = chain["p"].copy()
        sta = time.time()
        for i in range(self.iter+self.burn):
            if i % self.logint == 0 and i != 0:
                timer = (time.time()-sta) *((self.iter + self.burn - i)/self.logint)
                min = timer // 60
                sec = timer % 60
                info = 'Done %i / %i, remain est. : %i mins %i secs' %(i, (self.iter+self.burn), min, sec  )
                logging.info(info)
                print(info)
                sta = time.time()
                #sta = time.time()
            for pars, vals in self.save.items():   ## For Each parameters m , p , q etc..
                for par in range(len(vals)):            ## For Each generations m1, m2, m3 ...
                    self.new[pars][par] = np.max([norm.rvs(vals[par],self.shock[pars]),0.001])
                    probab = np.exp( self.posterior(self.new, self.xgs) - self.posterior(self.save,self.xgs) )
                    if np.isnan(probab):
                        probab = 0
                    if uniform.rvs(0,1) < probab:
                        self.save = copy.deepcopy(self.new)
                    if i >= self.burn:
                        s = i - self.burn
                        chain[pars][par,s] = self.save[pars][par]
            if i >= self.burn:
                chain["pos"][0,s] = probab
                chain["lh"][0,s] = self.likelihood(self.save,self.xgs)["sumll"]
        save_object(chain,"chain.p")
        return chain

    def bass_estimate(self, params):
        pass

    def chain2params(self,chain):
        params = {}
        params["m"] = []
        if self.fixp:
            params["p"] = [chain["p"][0,-1]]
        else:
            params["p"] = []
        params["sd"] = [chain["sd"][0, -1]]
        params["q"] = []
        for i in range(self.l):
            params["m"].append( chain["m"][i,-1] )
            params["q"].append( chain["q"][i,-1] )
            if self.fixp is False:
                params["p"].append(chain["p"][i,-1])
        return params
        #def predict_by_posterior(self, params):

    def xg(self,price = None, xg = None):
        if price is None: price = self.price
        assert xg is not None, "xg must have a value."
    def plot_mat(self,mat,path=None,title=None,total=False):
        plt.subplots_adjust(wspace=0.4, hspace= 0.6)
        sub = []
        x = list(range(mat.shape[1]))
        if len(x) < 9:
            h = 3
        else:
            h = 4
        for i in range(mat.shape[0]):
            sub.append(plt.subplot(h,3,i+1))
            if title is not None:
                sub[i].set_title("%s %i" % (title, i+1))
            sub[i].plot(x,mat[i,:])
        if total:
            sub.append(plt.subplot(h,3,mat.shape[0]+1))
            sub[mat.shape[0]].grid()
            sub[mat.shape[0]].set_title("%s total" % title)
            sub[mat.shape[0]].plot(x,mat.sum(axis=0))
        if path is None:
            plt.show()
            plt.clf()

        else:
            plt.savefig(path)
            plt.clf()





if __name__ == "__main__":
    #sb = SBass(sales=SALES, generations=GENERATION, prior=PRIOR,shock = SHOCK,burn=0,ite=3000,log_interval=500,fixp=False)
    #print(sb.new)
    #print(sb.bass_pred(xgs=sb.xgs, params = sb.new))
    #print(sb.posterior(sb.new,sb.xgs))

    #res = sb.MCMC()
    #print(result)
    #res = load_object('chain.p')
    #params = sb.chain2params(res)
    #sb = SBass(sales=SALES[0:10], generations=[0,3], prior=PRIOR, shock=SHOCK, burn=0, ite=3000, log_interval=500,
    #           fixp=True)
    sb = SBass(sales=np.array(SALES)/10,
               generations=GENERATION, prior=PRIOR, shock=SHOCK,
               burn=0, ite=3000, log_interval=500,
               fixp=True)

    #res = sb.MCMC()
    res = load_object('chain.p')
    params = sb.chain2params(res)

    """
    print(params)
    #params = {"m":[7, 10],"p":[0.1], "q":[0.5, 0.3]}
    ss = sb.bass_pred(params,[list(range(10)),list(range(7))],t = 10 )
    print(ss)
    y = np.sum(ss,axis=0)
    print(y)
    for i in y:
        print("%.3f"% i, end=", ")
    t = SALES2
    rmse = np.sqrt(np.sum((np.array(t) - y) ** 2) / 10 )
    print(rmse)
    plt.plot(list(range(len(SALES2))), t)
    plt.grid()
    plt.plot(list(range(10)), y, "-o")
    plt.plot(list(range(10)), ss[0,:])
    plt.plot(list(range(10)), ss[1, :])
    plt.xlabel("Time Period")
    plt.ylabel("Sales")
    plt.show()
    """

    """
    sub1 = plt.subplot(1,2,1)
    sub2 = plt.subplot(1, 2, 2)
    sub1.plot(list(range(10)), y)
    sub1.grid()
    sub1.set_title("m = 10, p = 0.1, q = 0.6")
    params2 = {"m": [10], "p": [0.6], "q": [0.1]}
    y2 = np.sum( sb.bass_pred(params2,[list(range(10))]),axis=0 )
    print(y2)
    sub2.plot(list(range(10)), y2)
    sub2.set_title("m = 10, p = 0.6, q = 0.1")
    sub2.grid()

    plt.show()
    """



    print(params)

    leap = sb.bass_pred(params,sb.xgs,rleap=True)
    print(leap)
    sb.plot_mat(leap,path="leap.png", title="Leapfrog",total=True)
    sb.plot_mat(res["m"],"m","m")

    xaxis = list(range(res["m"].shape[1]))
    x = list(range(res["m"].shape[1]))
    y = res["m"][0,:]

    plt.figure(figsize=(10, 10))
    sub1 = plt.subplot(3, 3, 1)
    sub2 = plt.subplot(3, 3, 2)
    sub3 = plt.subplot(3, 3, 3)
    sub4 = plt.subplot(3, 3, 4)
    sub5 = plt.subplot(3, 3, 5)
    sub6 = plt.subplot(3, 3, 6)
    sub7 = plt.subplot(3, 3, 7)
    sub8 = plt.subplot(3, 3, 8)
    sub9 = plt.subplot(3, 3, 9)
    sub2.grid()
    sub3.grid()
    sub4.grid()
    sub5.grid()
    sub6.grid()
    sub7.grid()
    sub8.grid()
    sub9.grid()

    sub1.grid()
    sub1.plot(x,y,"-x")

    x2 = list(range(res["m"].shape[1]))
    y2 = res["m"][1,:]
    #.show()
    sub2.plot(x2, y2, "-x")

    x3 = list(range(res["m"].shape[1]) )
    y3 = res["sd"][0,:]
    sub3.plot(x3, y3, "-x")

    x4 = xaxis
    y4 = res["q"][0,:]

    sub4.plot(x4, y4, "-x")

    x5 = xaxis
    y5 = res["q"][1, :]

    sub5.plot(x5, y5, "-x")

    x6 = xaxis
    y6 = res["p"][0, :]

    sub6.plot(x6, y6, "-x")

    x7 = xaxis
    y7 = res["pos"][0, :]

    sub7.plot(x7, y7, "-x")

    x8 = xaxis
    y8 = res["lh"][0, :]

    sub8.plot(x8, y8, "-x")

    ss = sb.bass_pred(params,sb.xgs)
    s = np.sum(ss,axis=0)

    x = range(sb.t)
    y1 = sb.sales
    y2 = s

    sub9.plot(x,y1, "-o")
    sub9.plot(x,y2, "-x")
    for i in range(ss.shape[0]):
        sub9.plot(x,ss[i,:])

    plt.show()
    plt.plot(x,y1, "-o")
    plt.plot(x,y2, "-x")
    for i in range(ss.shape[0]):
        plt.plot(x,ss[i,:])
    plt.legend()
    plt.show()
    print(ss)
    print(ss)




