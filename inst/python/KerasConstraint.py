import tensorflow.compat.v2 as tf
from keras import backend
import numpy as np

class KerasConstraint:
    def __init__(self, order=1):
        self.order = order

    def __call__(self, layer):
        layer.set_weights(self.__rescale_np(layer.get_weights()))

    def __rescale_np(self, wb):
        wb = np.vstack(wb)
        norms = np.linalg.norm(wb, ord=self.order, axis=0, keepdims=True)
        desired = np.clip(norms, 0, 1)
        wb = wb * (desired / (backend.epsilon() + norms))
        wb = np.vsplit(wb, [wb.shape[0] - 1])
        wb[1] = np.squeeze(wb[1])
        return wb

    # this one seems to be slower (because of the GPU?)
    def __rescale_tf(self, wb):
        wb[1] = tf.expand_dims(wb[1], axis=0)
        wb = tf.concat(wb, axis=0)
        norms = tf.norm(wb, ord=self.order, axis=0, keepdims=True)
        desired = backend.clip(norms, 0, 1)
        wb = wb * (desired / (backend.epsilon() + norms))
        wb = tf.split(wb, [wb.shape[0] - 1, 1])
        wb[1] = tf.squeeze(wb[1], axis=0)
        return wb

    def get_config(self):
        return {"order": self.order}
