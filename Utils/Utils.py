import pickle

def load_object( p):
    return pickle.load(open(p, "rb"))

def save_object( obj, p):
    pickle.dump(obj, open(p, "wb"))
