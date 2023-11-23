import keras

class KerasCallback(keras.callbacks.Callback):
    def __init__(self, constraint):
        super().__init__()
        self.constraint = constraint

    def on_train_batch_end(self, batch, logs=None):
        for layer in self.model.layers[:-1]:
            self.constraint(layer)
