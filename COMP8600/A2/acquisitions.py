import numpy as np
from scipy.stats import norm

# Functional Structure


def probability_improvement(X: np.ndarray, X_sample: np.ndarray,
                            gpr: object, xi: float = 0.01) -> np.ndarray:
    """
    Probability improvement acquisition function.

    Computes the PI at points X based on existing samples X_sample using
    a Gaussian process surrogate model

    Arguments:
    ----------
        X: ndarray of shape (m, d)
            The point/points for which the expected improvement needs to be computed.

        X_sample: ndarray of shape (n, d)
            Sample locations

        gpr: GPR object.
            Gaussian process trained on previously evaluated hyperparameters.

        xi: float. Default 0.01
            Exploitation-exploration trade-off parameter.

    Returns:
    --------
        PI: ndarray of shape (m,)
    """
    # TODO Q2.5
    # Implement the probability of improvement acquisition function

    # FIXME
    mean, std = gpr.predict(X, True)
    best_u = np.max(gpr.predict(X_sample))
    m, d = mean.shape
    Z = (mean - best_u - xi) / std
    ProbI = norm.cdf(Z)
    return ProbI.reshape(m,d).flatten()


def expected_improvement(X: np.ndarray, X_sample: np.ndarray,
                         gpr: object, xi: float = 0.01) -> np.ndarray:
    """
    Expected improvement acquisition function.

    Computes the EI at points X based on existing samples X_sample using
    a Gaussian process surrogate model

    Arguments:
    ----------
        X: ndarray of shape (m, d)
            The point/points for which the expected improvement needs to be computed.

        X_sample: ndarray of shape (n, d)
            Sample locations

        gpr: GPR object.
            Gaussian process trained on previously evaluated hyperparameters.

        xi: float. Default 0.01
            Exploitation-exploration trade-off parameter.

    Returns:
    --------
        EI : ndarray of shape (m,)
    """

    # TODO Q2.5
    # Implement the expected improvement acquisition function

    # FIXME
    mean, std = gpr.predict(X, True)
    best_u = np.max(gpr.predict(X_sample))
    Z = (mean - best_u - xi) / std
    EI = (mean - best_u - xi) * norm.cdf(Z) + std * norm.pdf(Z)
    m,d = mean.shape
    EI = EI.reshape(m,d)
    EI[np.where(std == 0.0)] = 0.0
    return EI.flatten()

