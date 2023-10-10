from __future__ import annotations

from kernels import Matern
import scipy.linalg as slinalg
from scipy.optimize import minimize
from typing import Callable, Tuple, Union, Type
import numpy as np

# Class Structure


class GPR:
    """
    Gaussian process regression (GPR).

    Arguments:
    ----------
    kernel : kernel instance, 
        The kernel specifying the covariance function of the GP. 

    noise_level : float , default=1e-10
        Value added to the diagonal of the kernel matrix during fitting.
        It can be interpreted as the variance of additional Gaussian
        measurement noise on the training observations. 

    n_restarts : int, default=0
        The number of restarts of the optimizer for finding the kernel's
        parameters which maximize the log-marginal likelihood. The first run
        of the optimizer is performed from the kernel's initial parameters,
        then the hyperparameters are sampled log-uniform randomly
        (for more details: https://en.wikipedia.org/wiki/Reciprocal_distribution)
        from the space of allowed hyperparameter-values. If greater than 0, all bounds
        must be finite. Note that `n_restarts == 0` implies that one
        run is performed.

    random_state : RandomState instance
    """

    def __init__(self,
                 kernel: Matern,
                 noise_level: float = 1e-10,
                 n_restarts: int = 0,
                 random_state: Type[np.random.RandomState] = np.random.RandomState
                 ) -> None:

        self.kernel = kernel
        self.noise_level = noise_level
        self.n_restarts = n_restarts
        self.random_state = random_state(4)

    def optimisation(self,
                     obj_func: Callable,
                     initial_theta: np.ndarray,
                     bounds: Tuple
                     ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Function that performs Quasi-Newton optimisation using L-BFGS-B algorithm.

        Note that we should frame the problem as a minimisation despite trying to
        maximise the log marginal likelihood.

        Arguments:
        ----------
        obj_func : the function to optimise as a callable
        initial_theta : the initial theta parameters, use under x0
        bounds : the bounds of the optimisation search

        Returns:
        --------
        theta_opt : the best solution x*
        func_min : the value at the best solution x, i.e, p*
        """
        # TODO Q2.3
        # Implement an L-BFGS-B optimisation algorithm using scipy.minimize built-in function

        # FIXME
        negate_func = lambda x: (-1 * obj_func(x))
        opt = minimize(fun = negate_func, x0 = initial_theta,
                       method ="L-BFGS-B", bounds = bounds)
        return opt.x,  -opt.fun

    def update(self, X: np.ndarray, y: np.ndarray) -> GPR:
        """
        Update Gaussian process regression model's parameters. You can get the bounds from the
        kernel function. Run the update for n_restarts. This means for each run we sample an initial
        pair for values theta and compute the log likelihood. A restart means that we resample values 
        of theta and run the process again (see __init__). Finally, choose the values theta which induce the best
        log likelihood.

        Arguments:
        ----------
        X : ndarray of shape (n_samples, n_features)
            Feature vectors or other representations of training data.
        y : ndarray of shape (n_samples, n_targets)
            Target values.

        Returns:
        --------
        self : object
            The current GPR class instance.
        """
        # TODO Q2.3
        # Fit the Gaussian process by performing hyper-parameter optimisation
        # using the log marginal likelihood solution. To maximise the log marginal
        # likelihood, you should use the `optimisation` function

        # HINT I: You should run the optimisation (n_restarts) time for optimum results.

        # HINT II: We have given you a data structure for all log-transformed `theta` hyper-parameters,
        #           coming from the Matern class. You can assume by optimising `theta` you are optimising
        #           all the hyper-parameters.

        # HINT III: Implementation detail - Since `theta` contains the log-transformed hyperparameters
        #               of the kernel, so now we are operating on a log-space. So your sampling distribution
        #               should be uniform.
        self.X_train = X
        self.y_train = y

        # FIXME
        for i in range(self.n_restarts+1):
            if i == 0:
                thetaopt, log_likeopt = self.optimisation(obj_func = self.log_marginal_likelihood, 
                                                    initial_theta = self.kernel.get_theta(), 
                                                    bounds = self.kernel.get_bounds())
            else:
                new_initial = np.random.uniform(self.kernel.get_bounds()[:,0], self.kernel.get_bounds()[:,1], 2)
                new_optheta, new_log_like = self.optimisation(obj_func = self.log_marginal_likelihood, 
                                                            initial_theta = new_initial, 
                                                            bounds = self.kernel.get_bounds())
                if new_log_like > log_likeopt:
                    log_likeopt = new_log_like
                    thetaopt = new_optheta
        self.kernel.set_theta(thetaopt)
        return self

    def predict(self, X: np.ndarray, return_std: bool = False) -> Union[np.ndarray, Tuple[np.ndarray, np.ndarray]]:
        """
        Predict using the Gaussian process regression model.

        In addition to the mean of the predictive distribution, optionally also
        returns its standard deviation (`return_std=True`).

        To incorporate noisy observations we need to add the noise to the diagonal 
        of the covariance K.

        Arguments:
        ----------
        X : ndarray of shape (n_samples, n_features)
            Query points where the GP is evaluated.
        return_std : bool, default=False
            If True, the standard-deviation of the predictive distribution at
            the query points is returned along with the mean.

        Returns (depending on the case):
        --------------------------------
        y_mean : ndarray of shape (n_samples, n_targets)
            Mean of predictive distribution a query points.
        y_std : ndarray of shape (n_samples, n_targets), optional
            Standard deviation of predictive distribution at query points.
            Only returned when `return_std` is True.
        """
        # TODO Q2.3
        # Implement the predictive distribution of the Gaussian Process Regression
        # by using the Algorithm (2) from the assignment sheet.

        # FIXME
        if len(self.y_train.shape)>=2:
            N, d = self.y_train.shape
        else:
            N = self.y_train.shape[0]
            d = 1
        m = X.shape[0]
    
        # Kernels
        K_X_X = self.kernel(self.X_train)
        K_X_X = K_X_X + (self.noise_level) * np.eye(N)
        
        K_x_star_X = self.kernel(X, self.X_train)
        
        # Use Cholesky decomposition
        L = np.linalg.cholesky(K_X_X)
        
        # Find alpha (solution to K alpha = t)
        alpha = np.linalg.solve(L.T, np.linalg.solve(L, self.y_train))
        m_x_star = K_x_star_X @ alpha 
        
        if return_std == True:
            K_x_star_x_star = self.kernel(X, X)
            v = np.linalg.solve(L, K_x_star_X.T) 
            var = np.diag(K_x_star_x_star)
            # use the Einstein notation
            var = var - np.einsum("ij,ji->i", v.T, v)
            return m_x_star.reshape((m,d)), np.sqrt(var).reshape((m,d))
        else:
            return m_x_star.reshape((m,d))

    def log_marginal_likelihood(self, theta: np.ndarray) -> float:
        """
        Return log-marginal likelihood of theta for training data.

        To incorporate noisy observations we need to add the noise to the diagonal 
        of the covariance K.

        Arguments:
        ----------
        theta : ndarray of shape (n_kernel_params,)
            Kernel hyperparameters for which the log-marginal likelihood is
            evaluated.

        Returns:
        --------
        log_likelihood : float
            Log-marginal likelihood of theta for training data.
        """
        # TODO Q2.3
        # Update the log-transformed hyperparameters (theta) and then 
        # compute the log marginal likelihood by using the Algorithm (2) from the assignment sheet.

        # FIXME
        N = self.X_train.shape[0]
        self.kernel.set_theta(theta)
        K_X_X = self.kernel(self.X_train) + (self.noise_level) * np.eye(N)
        L = np.linalg.cholesky(K_X_X)
        alpha = np.linalg.solve(L.T, np.linalg.solve(L, self.y_train))
        loglik = - np.sum(np.log(np.diag(L))) -0.5 * np.dot(self.y_train.T, alpha) - (N / 2) * np.log(2 * np.pi)


        return loglik.sum().item()

