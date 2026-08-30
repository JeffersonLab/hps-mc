import torch 
from torch import nn, optim
import torch.nn.functional as F
from torch.utils.data import TensorDataset, DataLoader
import numpy as np
import pandas as pd

# Mass of Kaon in GeV
m_K = 0.493677

# compute adversary labels from dataframe
def get_InvM(df):

    # Create temporary dataframe and compute invariant mass
    df_temp = pd.DataFrame()
    df_temp['e_Ptot'] = np.sqrt(df['ele_Px']**2 + df['ele_Py']**2 + df['ele_Pz']**2)
    df_temp['p_Ptot'] = np.sqrt(df['pos_Px']**2 + df['pos_Py']**2 + df['pos_Pz']**2)
    df_temp['e_SVT_E_K'] = m_K / np.sqrt ( 1 - ( df_temp.e_Ptot**2 / (df_temp.e_Ptot**2  + m_K**2) ) )
    df_temp['p_SVT_E_K'] = m_K / np.sqrt ( 1 - ( df_temp.p_Ptot**2 / (df_temp.p_Ptot**2  + m_K**2) ) )
    df_temp['M'] = np.sqrt((df_temp.e_SVT_E_K+df_temp.p_SVT_E_K)**2 - (df.ele_Px+df.pos_Px)**2 - (df.ele_Py+df.pos_Py)**2 - (df.ele_Pz+df.pos_Pz)**2)

    # Return a one-hot encoding of the quantile bins
    return df_temp.M.to_numpy()

def get_one_hot_edges(vals, n_bins=5):
    vals = np.asarray(vals)

    # compute percentiles from 0..100 in (n_bins+1) steps -> n_bins intervals
    quantiles = np.linspace(0, 100, n_bins + 1)
    edges = np.percentile(vals, quantiles)   # length = n_bins + 1

    return edges

def get_adv_labels(vals, edges):
    vals = np.asarray(vals)

    bin_idx = np.searchsorted(edges, vals, side='right') - 1
    bin_idx = np.clip(bin_idx, 0, len(edges) - 2).astype(int)

    one_hot = np.eye(len(edges)-1)[bin_idx]   # shape (N, n_bins)
    return one_hot.astype(np.float32)

# Training Loop
def train_clas(dataloader, model, loss_fn, optimizer, scheduler_clas, device, print_results = True):
    size = len(dataloader.dataset)
    num_batches = len(dataloader)
    model.train()
    train_loss = 0
    
    for batch, (X, y) in enumerate(dataloader):
        
        X, y = X.to(device), y.to(device)
    
        # Compute prediction error
        pred = model(X)
        loss = loss_fn(pred, y)
        
        # Backpropagation
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()
        scheduler_clas.step()
            
        train_loss += loss.item()
            
        # if batch % 100 == 0:
        #     loss, current = loss.item(), batch * len(X)
        #     print(f"Current batch training loss: {loss:>7f}  [{current:>5d}/{size:>5d}]")
            
    train_loss /= num_batches
    if print_results:
        print(f"Training loss: {train_loss:>7f}")
    return(train_loss)


# Validation Loop
def validate_clas(dataloader, model, loss_fn, device, print_results = True):
    num_batches = len(dataloader)
    model.eval()
    val_loss = 0
    with torch.no_grad():
        for X, y in dataloader:

            X, y =  X.to(device), y.to(device)
            pred = model(X)
            val_loss += loss_fn(pred, y).item()
            
    val_loss /= num_batches
    if print_results:
        print(f"Validation loss: {val_loss:>7f} \n")
    return(val_loss)


# Testing Loop
def test_clas(dataloader, model, device):
    test_labels = np.array([])
    model.eval()
    with torch.no_grad():
        for X in dataloader:

            X = X[0]
            X = X.to(device)
            pred = model(X)
            test_labels = np.append( test_labels , pred.squeeze(1).cpu().numpy() )
            
    return(test_labels)


# Training Loop
# Training Loop
def train_adv(dataloader, classifier, adv, loss_fn, optimizer, scheduler_adv, device, print_results=True):
    size = len(dataloader.dataset)
    num_batches = len(dataloader)
    classifier.eval()
    adv.train()
    train_loss = 0
    
    for batch, (X, y, w) in enumerate(dataloader):
        
        X, y, w = X.to(device), y.to(device), w.to(device)

        #Ensure not all weights are zero
        if w.sum().item() == 0: continue

        # Forward pass through classifier
        with torch.no_grad():
            p = classifier(X)

        # Compute prediction error
        pred = adv(p)
        sample_losses = loss_fn(pred, y) # since we set reduction='none', this is now a vector of losses, one per sample in the batch

        # Weighted mean loss over non-zero weights
        nonzero = w > 0
        loss = (sample_losses[nonzero] * w[nonzero]).sum() / w[nonzero].sum()
        
        # Backpropagation
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()
        scheduler_adv.step()
            
        train_loss += loss.item()
            
        # if batch % 100 == 0:
        #     loss, current = loss.item(), batch * len(X)
        #     print(f"Current batch training loss: {loss:>7f}  [{current:>5d}/{size:>5d}]")
            
    train_loss /= num_batches
    if print_results:
        print(f"Training loss: {train_loss:>7f}")
    return(train_loss)


# Training Loop
def validate_adv(dataloader, classifier, adv, loss_fn, device, print_results=True):
    num_batches = len(dataloader)
    classifier.eval()
    adv.eval()
    val_loss = 0

    with torch.no_grad():
        for batch, (X, y, w) in enumerate(dataloader):

            X, y, w = X.to(device), y.to(device), w.to(device)
            
            #Ensure not all weights are zero
            if w.sum().item() == 0: continue

            p = classifier(X)

            pred = adv(p)
            sample_losses = loss_fn(pred, y) # since we set reduction='none', this is now a vector of losses, one per sample in the batch
            
            # Weighted mean loss over non-zero weights
            nonzero = w > 0
            loss = (sample_losses[nonzero] * w[nonzero]).sum() / w[nonzero].sum()

            val_loss += loss.item()
        
    val_loss /= num_batches
    if print_results:
        print(f"Validation loss: {val_loss:>7f} \n")
    return(val_loss)

# Testing Loop
def test_adv(dataloader, clas, adv, n_classes, device):
    adv_pred = np.empty(n_classes)
    clas.eval()
    adv.eval()
    with torch.no_grad():
        for X, y in dataloader:
            X, y =  X.to(device), y.to(device)
            pred = adv(clas(X))
            adv_pred = np.vstack([adv_pred,pred.cpu().numpy()])
            
    return(adv_pred[1:])


def set_requires_grad(model, requires_grad):
    for p in model.parameters():
        p.requires_grad = requires_grad


def train_full(dataloader, classifier, adv, loss_fn_clas, lambda_, loss_fn_adv, optimizer_clas, optimizer_adv, scheduler_clas, scheduler_adv, device, print_results=True):
    size = len(dataloader.dataset)
    num_batches = len(dataloader)
    train_loss_clas = 0
    train_loss_adv = 0
    classifier.train()
    adv.train()
    
    for batch, (X, y_clas, y_adv, w) in enumerate(dataloader):
        
        X, y_clas, y_adv, w = X.to(device), y_clas.to(device), y_adv.to(device), w.to(device)

        #Ensure not all weights are zero
        if w.sum().item() == 0: continue

        # -------------------------
        # 1) Classifier update (adv parameters frozen)
        # -------------------------
        set_requires_grad(adv, False)          # freeze adv params
        set_requires_grad(classifier, True)    # ensure classifier grads enabled

        # forward classifier
        class_out = classifier(X)

        # forward adversary using classifier outputs
        # do NOT detach adv input here because we want gradients to flow into classifier
        adv_out_for_clas = adv(class_out)

        # compute classifier loss
        loss_clas = loss_fn_clas(class_out, adv_out_for_clas, y_clas, y_adv, w, lambda_)

        # backprop and update classifier
        optimizer_clas.zero_grad()
        loss_clas.backward()
        optimizer_clas.step()
        scheduler_clas.step()

        train_loss_clas += loss_clas.item()

        # -------------------------
        # 2) Adversary update (classifier parameters frozen)
        # -------------------------
        
        set_requires_grad(classifier, False)   # freeze classifier params
        set_requires_grad(adv, True)           # ensure adv grads enabled

        # forward classifier and detach outputs so gradients do NOT flow into classifier
        # another forward pass through classifier is needed here to catch updated classifier weights
        class_out_detached = classifier(X).detach()

        # forward adv using detached classifier outputs
        adv_out = adv(class_out_detached)

        # compute adversary loss
        sample_losses_adv = loss_fn_adv(adv_out, y_adv) # since we set reduction='none', this is now a vector of losses, one per sample in the batch

        # Weighted mean loss over non-zero weights
        nonzero = w > 0
        loss_adv = (sample_losses_adv[nonzero] * w[nonzero]).sum() / w[nonzero].sum()

        optimizer_adv.zero_grad()
        loss_adv.backward()
        optimizer_adv.step()
        scheduler_adv.step()

        train_loss_adv += loss_adv.item()


    train_loss_clas /= num_batches
    train_loss_adv /= num_batches

    if print_results:
        print(f"Classifier training loss: {train_loss_clas:>7f}")
        print(f"Adversary training loss: {train_loss_adv:>7f}")
    
    return(train_loss_clas, train_loss_adv)


def train_full_orig(dataloader, classifier, adv, loss_fn_clas, lambda_, loss_fn_adv, optimizer_clas, optimizer_adv, scheduler_clas, scheduler_adv, adv_steps, device, grad_clip = False, print_results=True):
    num_batches = len(dataloader)
    train_loss_clas = 0
    train_loss_adv = 0
    classifier.train()
    adv.train()
    
    for batch, (X, y_clas, y_adv, w) in enumerate(dataloader):
        
        X, y_clas, y_adv, w = X.to(device), y_clas.to(device), y_adv.to(device), w.to(device)

        #Ensure not all weights are zero
        if w.sum().item() == 0: continue

        # -------------------------
        # 1) Classifier update (adv parameters frozen)
        # -------------------------
        set_requires_grad(adv, False)          # freeze adv params
        set_requires_grad(classifier, True)    # ensure classifier grads enabled

        # forward classifier
        class_out = classifier(X)

        # forward adversary using classifier outputs
        # do NOT detach adv input here because we want gradients to flow into classifier
        adv_out_for_clas = adv(class_out)

        # compute classifier loss
        loss_clas = loss_fn_clas(class_out, adv_out_for_clas, y_clas, y_adv, w, lambda_)

        # backprop and update classifier
        optimizer_clas.zero_grad()
        loss_clas.backward()
        if grad_clip:
            torch.nn.utils.clip_grad_norm_(classifier.parameters(), max_norm=0.1)
        optimizer_clas.step()
        scheduler_clas.step()

        train_loss_clas += loss_clas.item()

        # -------------------------
        # 2) Adversary update (classifier parameters frozen)
        # -------------------------
        
        set_requires_grad(classifier, False)   # freeze classifier params
        set_requires_grad(adv, True)           # ensure adv grads enabled

        # This follows the original prescription where the adv training loop is nested inside the classifier training loop
        # adv_steps specifies how many batches to use for adv training per classifier batch
        for batch, (X_i, y_clas_i, y_adv_i, w_i) in enumerate(dataloader):        

            # forward classifier and detach outputs so gradients do NOT flow into classifier
            class_out_detached = classifier(X_i).detach()

            # forward adv using detached classifier outputs
            adv_out = adv(class_out_detached)

            # compute adversary loss
            sample_losses_adv = loss_fn_adv(adv_out, y_adv_i) # since we set reduction='none', this is now a vector of losses, one per sample in the batch

            # Weighted mean loss over non-zero weights
            nonzero = w_i > 0
            loss_adv = (sample_losses_adv[nonzero] * w_i[nonzero]).sum() / w_i[nonzero].sum()

            optimizer_adv.zero_grad()
            loss_adv.backward()
            optimizer_adv.step()

            train_loss_adv += loss_adv.item()

            if batch == adv_steps-1:
                break

        scheduler_adv.step()

    train_loss_clas /= num_batches
    train_loss_adv /= (num_batches*adv_steps)

    if print_results:
        print(f"Classifier training loss: {train_loss_clas:>7f}")
        print(f"Adversary training loss: {train_loss_adv:>7f}")
    
    return(train_loss_clas, train_loss_adv)

def validate_full(dataloader, classifier, adv, loss_fn_clas, lambda_, loss_fn_adv, device, print_results = True):
    size = len(dataloader.dataset)
    num_batches = len(dataloader)
    val_loss_clas = 0
    val_loss_adv = 0
    classifier.eval()
    adv.eval()

    with torch.no_grad():
    
        for batch, (X, y_clas, y_adv, w) in enumerate(dataloader):
            
            X, y_clas, y_adv, w = X.to(device), y_clas.to(device), y_adv.to(device), w.to(device)

            #Ensure not all weights are zero
            if w.sum().item() == 0: continue

            # forward classifier
            class_out = classifier(X)

            # forward adversary
            adv_out = adv(class_out)

            # compute classifier loss
            loss_clas = loss_fn_clas(class_out, adv_out, y_clas, y_adv, w, lambda_)
            val_loss_clas += loss_clas.item()

            # compute adversary loss
            sample_losses_adv = loss_fn_adv(adv_out, y_adv) # since we set reduction='none', this is now a vector of losses, one per sample in the batch

            # Weighted mean loss over non-zero weights
            nonzero = w > 0
            loss_adv = (sample_losses_adv[nonzero] * w[nonzero]).sum() / w[nonzero].sum()

            val_loss_adv += loss_adv.item()


    val_loss_clas /= num_batches
    val_loss_adv /= num_batches

    if print_results:
        print(f"Classifier validation loss: {val_loss_clas:>7f}")
        print(f"Adversary validation loss: {val_loss_adv:>7f}")
    
    return(val_loss_clas, val_loss_adv)


# Testing Loop
def test_final(dataloader, model, device):
    test_labels = np.array([])
    model.eval()
    with torch.no_grad():
        for X in dataloader:

            X =  X[0].to(device)
            pred = model(X)
            test_labels = np.append( test_labels , pred.squeeze(1).cpu().numpy() )
            
    return(test_labels)


def get_diff_score(x1,x2):

    N, edges = np.histogram(x1, bins=np.arange(0,300,1), density=False)
    M, edges = np.histogram(x2, bins=np.arange(0,300,1), density=False)

    N2 = N[(N > 0) & (M > 0)]
    M2 = M[(N > 0) & (M > 0)]

    diff_score = np.mean( np.abs( N2/np.sum(N) - M2/np.sum(M) ) / np.sqrt( ( np.sqrt(N2)/np.sum(N) )**2 + ( np.sqrt(M2)/np.sum(M) )**2 ) )

    return diff_score


# simple MLP for binary classification (single logit output)
class Classifier(nn.Module):
    def __init__(self, in_features, hidden1=264, hidden2=264, hidden3=128, hidden4=128, hidden5=64):
        super().__init__()
        self.net = nn.Sequential(
            nn.Linear(in_features, hidden1,bias=False),
            nn.BatchNorm1d(hidden1),
            nn.ReLU(),
            nn.Linear(hidden1, hidden2,bias=False),
            nn.BatchNorm1d(hidden2),
            nn.ReLU(),
            nn.Linear(hidden2, hidden3,bias=False),
            nn.BatchNorm1d(hidden3),
            nn.ReLU(),
            nn.Linear(hidden3, hidden4,bias=False),
            nn.BatchNorm1d(hidden4),
            nn.ReLU(),
            nn.Linear(hidden4, hidden5,bias=False),
            nn.BatchNorm1d(hidden5),
            nn.ReLU(),
            nn.Linear(hidden5, 1),
        )
    def forward(self, x):
        return self.net(x)
    

class Adversary(nn.Module):
    def __init__(self, n_classes, input_dim=1, hidden1=64, hidden2=128, hidden3=64):
        super().__init__()
        self.net = nn.Sequential(
            nn.Linear(input_dim, hidden1,bias=False),
            nn.BatchNorm1d(hidden1),
            nn.ReLU(),
            nn.Linear(hidden1, hidden2,bias=False),
            nn.BatchNorm1d(hidden2),
            nn.ReLU(),
            nn.Linear(hidden2, hidden3,bias=False),
            nn.BatchNorm1d(hidden3),
            nn.ReLU(),
            nn.Linear(hidden3, n_classes)   # outputs raw logits for CrossEntropyLoss
            )
        
    def forward(self, x):
        # make the shape (N,1) in a way that is robust for squeezed / unsqueezed classifier outputs
        x = x.view(x.size(0), -1)
        return self.net(x)  # returns shape (N, n_classes)

class Adversary_small(nn.Module):
    def __init__(self, n_classes, input_dim=1, hidden1=128):
        super().__init__()
        self.net = nn.Sequential(
            nn.Linear(input_dim, hidden1),
            nn.ReLU(),
            nn.Linear(hidden1, n_classes)   # outputs raw logits for CrossEntropyLoss
            )
        
    def forward(self, x):
        # make the shape (N,1) in a way that is robust for squeezed / unsqueezed classifier outputs
        x = x.view(x.size(0), -1)
        return self.net(x)  # returns shape (N, n_classes)

# simple MLP for binary classification (single logit output)
class Classifier_2021(nn.Module):
    def __init__(self, in_features, hidden1=264, hidden2=264, hidden3=128, hidden4=128, hidden5=128, hidden6=64):
        super().__init__()
        self.net = nn.Sequential(
            nn.Linear(in_features, hidden1,bias=False),
            nn.BatchNorm1d(hidden1),
            nn.ReLU(),
            nn.Linear(hidden1, hidden2,bias=False),
            nn.BatchNorm1d(hidden2),
            nn.ReLU(),
            nn.Linear(hidden2, hidden3,bias=False),
            nn.BatchNorm1d(hidden3),
            nn.ReLU(),
            nn.Linear(hidden3, hidden4,bias=False),
            nn.BatchNorm1d(hidden4),
            nn.ReLU(),
            nn.Linear(hidden4, hidden5,bias=False),
            nn.BatchNorm1d(hidden5),
            nn.ReLU(),
            nn.Linear(hidden5, hidden6,bias=False),
            nn.BatchNorm1d(hidden6),
            nn.ReLU(),
            nn.Linear(hidden6, 1),
        )
    def forward(self, x):
        return self.net(x)
    


QualCuts = {
    'pos_E_Ecal_low': 0.5,
    'pos_Pz_low': 0.65,
    'pos_Px_low': -0.06,
    'pos_Py_low': -0.12,
    'pos_Py_high': 0.13,
    'ele_Px_low': -0.12,
    'ele_Py_low': -0.14,
    'ele_Py_high': 0.16,
    'pos_Ecal_x_low': 100.0,
    'pos_Ecal_y_low': -85.0,
    'pos_Ecal_y_high': 90.0,
    'pos_Ecal_z_low': 1448.6,
    'ele_Ecal_x_high': 10.0,
    'ele_Ecal_z_low': 1448.6
}