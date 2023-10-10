###### DO NOT CHANGE ######

from abc import ABC, abstractmethod

import torch

class Model(ABC):

    @abstractmethod
    def make_random_parameter(self) -> torch.Tensor:
        pass

    @abstractmethod
    def predict(self, x: torch.Tensor, theta: torch.Tensor) -> torch.Tensor:
        """ f: X x Theta -> Rm
        """
        pass

class ExponentialFamily(ABC):

    @abstractmethod
    def log_partition(self, eta: torch.Tensor) -> float:
        """ psi: Rm x X -> R
        """
        pass

    @abstractmethod
    def sufficient_statistic(self, y: torch.Tensor) -> torch.Tensor:
        """ u: Rn x X -> Rm
        """
        pass